import Crawler.crawl
import org.jsoup.Jsoup

import java.net.URL

case class Page(url: String, title: Option[String], links: List[String] )

object Crawler:

  def crawl(url: String, max: Int = 5): List[Page] = {
    val page = fetch(url)
    page match {
      case Some(p) => p :: p.links.takeRight(max).flatMap(l => crawl(l, max - 1))
      case None => Nil
    }
  }

  def fetch(url: String): Option[Page] = try
    println(s"Fetching $url")
    import scala.collection.JavaConverters._

    val doc = Jsoup.parse(new URL(url), 20*1000)

    val title = doc.select("head title").text()
    val links = doc.select("a[href]")
      .asScala
      .toList
      .map(_.attr("abs:href"))

    Some(Page(url, Some(title), links))

    catch
      case e => None

object CrawlerDemo extends App:

  val pages = crawl("http://www.scala-lang.org")
  println(pages)