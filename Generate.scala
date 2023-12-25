//> using lib "org.commonmark:commonmark:0.21.0"
//> using lib "org.commonmark:commonmark-ext-yaml-front-matter:0.21.0"
//> using lib "com.hubspot.jinjava:jinjava:2.7.1"
//> using lib "com.lihaoyi::os-lib:0.9.2"
//> using lib "com.lihaoyi::cask:0.9.1"

import scala.io.Source
import scala.collection.immutable.Map
import scala.jdk.CollectionConverters.*
import java.io.File
import com.hubspot.jinjava.*
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

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

  def apply(input: String, ctx: Context): String = {
    val (content, metadata) = markdownToHtml(input)
    val template = injectIn(content, "page.html")
    jinjava.render(template, ctx.meld(metadata).toJavaMap)
  }
}

class Server(val root: os.Path) extends cask.MainRoutes {

  @cask.get("/", subpath = true)
  def get(request: cask.Request) = {
    val path = {
      val path = os.SubPath(request.remainingPathSegments.mkString("/"))
      if path.ext == "" then path / "index.html" else path
    }
    val contentType = path.ext match {
      case "html" => "text/html"
      case "css" => "text/css"
      case "svg" => "image/svg+xml"
      case _ => "text/plain"
    }
    try {
      cask.Response(
        data = os.read(root / path),
        headers = Seq("Content-Type" -> s"${contentType}; charset=utf-8")
      )
    }
    catch {
      _ => cask.Response("", statusCode = 404)
    }
  }

  initialize()
}

@main
def main(args: String*) = {
  val baseUrl = "https://ef5.ch"
  val contentDir = os.pwd / "content"
  val staticDir = os.pwd / "static"
  val outDir = os.pwd / ".site"
  val deployer = Deployer()

  os.remove.all(outDir)

  walkFiles(contentDir).filter(_.last == "_.md").foreach{path =>
    val relativeOriginal = path.subRelativeTo(contentDir)
    val relative = relativeOriginal / os.up
    val input = os.read(path)

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
            "About me" -> s"/",
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

    val output = deployer(input, ctx)
    os.write(outDir / relative / "index.html", output, createFolders = true)
  }

  walkFiles(staticDir).foreach{path =>
    val relative = path.relativeTo(staticDir)
    os.copy(path, outDir / relative, createFolders = true)
  }

  if (args.length > 0 && args(0) == "server") {
    val server = Server(outDir)
    server.main(Array.empty)
  }
}