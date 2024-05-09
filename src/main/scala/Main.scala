import org.jsoup._
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.util.control.Breaks._
import scala.util._
import java.io.PrintWriter
import java.io.File
import org.checkerframework.checker.units.qual.s

object Main {

  /** Scrap data from a web page using his url
    *
    * @param url
    * @return
    *   a mutable map that contains all significative data from the web page
    */
  def get_restaurant_infos(url: String): mutable.Map[String, String] = {
    try {
      val doc = Jsoup.connect(url).get()

      val restaurant_name = doc.select("div h1").text
      val restaurant_desc = doc.select("div#description div").text.trim()
      val restaurant_address = doc.select("div address").text.trim()

      val tels = doc.select("div.mb-3 a").text.split("[(]+[+]+228\\) ").drop(1)
      val restaurant_tels = tels.map(_.filterNot(_.isWhitespace)).mkString(",")

      val txt = doc.select("div ul li").text
      val restaurant_services = txt
        .substring(txt.lastIndexOf("Contact") + 8)
        .trim()

      var restaurant_opening_day = ""
      for (row <- doc.select("table tr").asScala) {
        restaurant_opening_day += row.text
          .split(" ")
          .zipWithIndex
          .filter(_._2 != 2)
          .map(_._1)
          .mkString("", "+", ",")
      }

      if (restaurant_opening_day.isBlank()) {
        restaurant_opening_day = doc.select("#pratical-info h4").text().trim()
      }

      return mutable.Map(
        "name" -> restaurant_name,
        "description" -> restaurant_desc,
        "address" -> restaurant_address,
        "tels" -> restaurant_tels,
        "services" -> restaurant_services,
        "opening day" -> restaurant_opening_day
      )
    } catch {
      case e: Throwable =>
        println(s"Error fetching data for URL: $url")
        return mutable.Map.empty[
          String,
          String
        ]
    }
  }

  /** Here is where the test of the function get_restaurant_infos() is done
    */
  def main(args: Array[String]): Unit = {
    val url =
      "https://www.goafricaonline.com/en/tg/result-directory?type=company&whatWho=restaurants&near=false&companySizes=%5B%5D"
    var restaurants = mutable.Map[String, String]()

    val file = new File("data.csv")
    val pw = new PrintWriter(file)
    pw.println("name,description,address,tels,services,opening_day")

    val doc = Jsoup.connect(url).get()

    for (row <- doc.select("article a:first-child").asScala.take(33)) {
      breakable {
        val txt = row.attr("href")
        if (txt.isEmpty()) break
        println(txt)
        restaurants = get_restaurant_infos(txt)
        if (!restaurants.isEmpty) {
          pw.println(
            s"\"${restaurants("name")}\", \"${restaurants("description")}\", \"${restaurants(
                "address"
              )}\", \"${restaurants("tels")}\", \"${restaurants("services")}\", \"${restaurants("opening day")}\""
          )
        }
      }
    }

    pw.close()
  }
}
