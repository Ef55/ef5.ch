#!/usr/bin/env scala-cli

//> using lib "org.commonmark:commonmark:0.22.0"
//> using lib "org.commonmark:commonmark-ext-yaml-front-matter:0.22.0"
//> using lib "com.hubspot.jinjava:jinjava:2.7.2"
//> using lib "com.lihaoyi::os-lib:0.10.0"
//> using lib "com.lihaoyi::cask:0.9.2"

import scala.io.Source
import scala.collection.immutable.Map
import scala.jdk.CollectionConverters.*
import java.io.File
import com.hubspot.jinjava.*
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.annotation.targetName

def walkFiles(root: os.Path) = {
  os.walk.stream.attrs(root, includeTarget = true).collect{
    case (path, infos) if infos.fileType != os.FileType.Dir => path
  }
}

def getGitContext(root: os.Path, path: os.SubPath): Context = {
  import scala.language.implicitConversions
  val output = os.proc("git", "rev-list", "-1", "--pretty=format:%h %ad", "--date=short", "HEAD", "--", root / path).call(cwd = root).out.lines().toList
  output match {
    case  s"commit ${commit}" ::
          s"${abrCommit} ${year}-${month}-${day}" ::
          nil =>  Context("hash" -> commit, "abbreviated_hash" -> abrCommit, "date" -> s"${day}.${month}.${year}")
    case Nil => Context("hash" -> "0000000000000000000000000000000000000000", "abbreviated_hash" -> "0000000", "date" -> s"01.01.1970")
    case _ => throw new RuntimeException(s"Unexpected git output:\n${output}")
  }
}

sealed trait Context {
  def toJavaValue: Object = {
    this match {
      case Struct(m) => m.view.mapValues(_.toJavaValue).toMap.asJava
      case StringValue(v) => v
      case BooleanValue(v) => Boolean.box(v)
    }
  }
  def toJavaMap: java.util.Map[String, Object] = {
    this match {
      case Struct(m) => m.view.mapValues(_.toJavaValue).toMap.asJava
      case _ => java.util.Map.of()
    }
  }

  def meld (that: Context): Context = {
    (this, that) match {
      case (Struct(m1), Struct(m2)) =>
        Struct(m1.foldLeft(m2)( (c, p) => {
          val (k, v1) = p
          c.updatedWith(k)(mv2 => {
            mv2 match {
              case Some(v2) => Some(v1.meld(v2))
              case None => Some(v1)
            }
          })
        }))
      case _ => throw new java.lang.RuntimeException(s"Cannot meld contexts: types are ${this.getClass} and ${that.getClass}.")
    }
  }

  def add(path: String*)(v: Context): Context = {
    this.meld(Context.singleton(path: _*)(v))
  }
  def addIfMissing(path: String*)(v: Context): Boolean = {
    contains(path: Seq[String])
  }
  @targetName("addIfMissingSeq")
  def addIfMissing(path: Seq[String])(v: Context): Boolean = {
    (this, path) match {
      case (Struct(m), h +: t) if m.contains(h) => m(h).contains(t)
      case (Struct(m), h +: t) if !m.contains(h) => m(h).contains(t)
      case _ => throw new java.lang.RuntimeException(s"")
    }
  }

  def contains(path: String*): Boolean = {
    contains(path: Seq[String])
  }
  @targetName("containsSeq")
  def contains(path: Seq[String]): Boolean = {
    (this, path) match {
      case (Struct(m), h +: t) if m.contains(h) => m(h).contains(t)
      case (_, Seq()) => true
      case _ => false
    }
  }
}
case class Struct(m: Map[String, Context]) extends Context 
case class StringValue(v: String) extends Context
case class BooleanValue(v: Boolean) extends Context
given Conversion[String, Context] with {
  def apply(v: String) = StringValue(v)
}
given Conversion[Boolean, Context] with {
  def apply(v: Boolean) = BooleanValue(v)
}
object Context {
  def singleton(path: String*)(v: Context): Context = {
    path.foldRight(v)((h, v) => Struct(Map(h -> v)))
  }

  def apply(ls: (String, Context)*) = {
    Struct(Map(ls: _*))
  }
}

def markdownToHtml(content: String): (String, Context) = {
  import scala.jdk.CollectionConverters.*
  import org.commonmark.node.*
  import org.commonmark.parser.Parser
  import org.commonmark.renderer.html.HtmlRenderer
  import org.commonmark.ext.front.matter.{ YamlFrontMatterExtension, YamlFrontMatterVisitor}

  val extensions = List(YamlFrontMatterExtension.create()).asJava
  val parser = Parser.builder()
    .extensions(extensions)
    .build()

  val renderer = HtmlRenderer.builder()
    .extensions(extensions)
    .build();

  val parsed = parser.parse(content)
  val metadata = YamlFrontMatterVisitor()
  parsed.accept(metadata)

  val metadataCtx = Context(
    "page" -> Struct(metadata.getData.asScala.view.mapValues(e => StringValue(e.asScala.mkString("|"))).toMap)
  )

  (renderer.render(parsed), metadataCtx)
}

def injectIn(content: String, template: String) = {
  s"""
  {% extends "${template}" %}
  {% block content %}
  ${content}
  {% endblock %}
  """
}

class Deployer {
  private val jinjava = new Jinjava();
  jinjava.setResourceLocator(loader.FileLocator(File("template")))

  lazy val handlers: Map[String, (String, Context) => String] = Map(
    "md" -> ((input, ctx0) => {
      val (content, metadata) = markdownToHtml(input)
      val mdCtx = Context(
        "page" -> Context(
          "stylesheet" -> StringValue("md.css"),
        )
      )
      val ctx = ctx0.meld(metadata).meld(mdCtx)
      val template = injectIn(content, "page.html")
      handle("html")(template, ctx)
    }),
    "html" -> ((input, ctx) => {
      jinjava.render(input, ctx.toJavaMap)
    })
  )

  def handles(kind: String): Boolean = {
    handlers.contains(kind)
  }

  def handle(kind: String)(input: String, ctx: Context): String = handlers(kind)(input, ctx)
}

class Server(val static: Map[os.SubPath, os.Path], val dynamic: Map[os.SubPath, Unit => String]) extends cask.MainRoutes {

  @cask.get("/", subpath = true)
  def get(request: cask.Request): cask.Response[Array[Byte]] = {
    val path = {
      val path = os.SubPath(request.remainingPathSegments.mkString("/"))
      if path.ext == "" then path / "index.html" else path
    }
    val contentType = path.ext match {
      case "html" => "text/html; charset=utf-8"
      case "css" => "text/css; charset=utf-8"
      case "svg" => "image/svg+xml"
      case "ttf" | "woff" | "woff2" => s"font/${path.ext}"
      case _ => "text/plain; charset=utf-8"
    }
    val mData: Option[Array[Byte]] = dynamic.get(path).map(_.apply(()).getBytes).orElse(static.get(path).map(os.read.bytes(_)))
    mData match {
      case None => cask.Response(Array.empty, statusCode = 404)
      case Some(data) => cask.Response(
          data = data,
          headers = Seq("Content-Type" -> contentType)
        )
    }
  }

  initialize()
}

@main
def main(args: String*) = {
  val baseUrl = "https://ef5.ch"
  val contentDir = os.pwd / "content"
  val staticDir = os.pwd / "static"
  val deployer = Deployer()

  val dynamic: Seq[(os.SubPath, Unit => String)] = walkFiles(contentDir)
    .filter(path => path.baseName == "_" && deployer.handles(path.ext))
    .map{path =>
      val relativeOriginal = path.subRelativeTo(contentDir)
      val relative = relativeOriginal / os.up

      import scala.language.implicitConversions
      val ctx = Context(
        "socials" -> Context(
          "Github" -> "https://github.com/Ef55"
        ),
        "site" -> Context(
          "url" -> baseUrl,
          "name" -> "Noé De Santo",
          "repo" -> "https://github.com/Ef55/ef5.ch",
          "menu" -> Context(
            "items" -> Context(
              "About me" -> "/me",
              "CV" -> "/cv",
            )
          ),
          "enable_katex" -> false,
          "git" -> getGitContext(contentDir, os.sub),
        ),
        "page" -> Context(
          "path" -> relative.toString,
          "git" -> getGitContext(contentDir, relativeOriginal),
        ),
      )

      val generate = (_: Unit) => {
        val input = os.read(path)
        deployer.handle(path.ext)(input, ctx)
      }
      (relative / "index.html", generate)
    }
    .toSeq

  val static: Seq[(os.SubPath, os.Path)] = walkFiles(staticDir)
    .map{path =>
      val relative = path.subRelativeTo(staticDir)
      (relative, path)
    }
    .toSeq

  if (args.length == 0 || (args.length == 1 && args(0) == "deploy")) {
    val outDir = os.pwd / ".site"
    os.remove.all(outDir)

    dynamic.foreach{ (path, generator) =>
      os.write(outDir / path, generator(()), createFolders = true)
    }
    static.foreach{ (path, source) =>
      os.copy(source, outDir / path, createFolders = true)
    }
  }
  else if (args.length == 1 && args(0) == "server") {
    val server = Server(static.toMap, dynamic.toMap)
    server.main(Array.empty)
  }
  else {
    println("Invalid arguments: " + args.mkString(" "))
  }
}