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

def markdownToHtml(content: String): (String, Map[String, List[String]]) = {
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
  (renderer.render(parsed), metadata.getData.asScala.view.mapValues(_.asScala.toList).toMap)
}

def injectIn(content: String, template: String) = {
  s"""
  {% extends "${template}" %}
  {% block content %}
  ${content}
  {% endblock %}
  """
}

sealed trait Context {
  def toJavaValue: Object = {
    this match {
      case Struct(m) => m.view.mapValues(_.toJavaValue).toMap.asJava
      case Value(v) => v
    }
  }
  def toJavaMap: java.util.Map[String, Object] = {
    this match {
      case Struct(m) => m.view.mapValues(_.toJavaValue).toMap.asJava
      case Value(v) => java.util.Map.of()
    }
  }
}
case class Struct(m: Map[String, Context]) extends Context 
case class Value(v: String) extends Context
given Conversion[String, Context] with {
  def apply(v: String) = Value(v)
}
object Context {
  def apply(ls: (String, Context)*) = {
    Struct(Map(ls: _*))
  }
}

class Deployer {
  private val jinjava = new Jinjava();
  jinjava.setResourceLocator(loader.FileLocator(File("template")))

  def apply(input: String, ctx: Map[String, List[String]] => Struct): String = {

    val (content, metadata) = markdownToHtml(input)
    val template = injectIn(content, "page.html")
    jinjava.render(template, ctx(metadata).toJavaMap)
  }
}

class Server(val root: os.Path) extends cask.MainRoutes {

  @cask.get("/", subpath = true)
  def get(request: cask.Request) = {
    val path = os.SubPath(request.remainingPathSegments.mkString("/"))
    val contentType = path.ext match {
      case "" | "html" => "text/html"
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
  val outDir = os.pwd / ".build"
  val deployer = Deployer()

  os.remove.all(outDir)

  walkFiles(contentDir).filter(_.last == "_.md").foreach{path =>
    val relative = path.relativeTo(contentDir) / os.up
    val input = os.read(path)

    import scala.language.implicitConversions
    val cal = java.util.Calendar.getInstance()
    val ctx: Map[String, List[String]] => Struct = metadata => Context(
      "date" -> s"${cal.get(java.util.Calendar.DATE)}.${cal.get(java.util.Calendar.MONTH) + 1}.${cal.get(java.util.Calendar.YEAR)}",
      "socials" -> Context(
        "Github" -> "https://github.com/Ef55"
      ),
      "site" -> Context(
        "url" -> baseUrl,
        "name" -> "Noé De Santo",
        "repo" -> "https://github.com/Ef55/website",
        "menu" -> Context(
          "items" -> Context(
            "About me" -> s"/",
          )
        ),
      ),
      "page" -> Context(
        "title" -> metadata.getOrElse("title", List.empty).mkString("|"),
      )
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