package controllers

import java.net.ConnectException
import javax.inject.Inject
import play.Logger
import play.api.libs.json.JsResultException
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.control.NonFatal

class Application @Inject() (theGuardianNewsFetcher: TheGuardianNewsFetcher) extends Controller {
  private val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  def index = Action.async {
    val s = System.currentTimeMillis()
    val allNews: Future[Map[String, Int]] = theGuardianNewsFetcher.fetchNewsFor("turkey", 90)
    allNews.map(response => {
      val sortedWords: Seq[(String, Int)] = response.toSeq.sortWith(_._2 > _._2)
      println(System.currentTimeMillis() - s)
      Ok(sortedWords.toString())
    })(ec).recover{
      case e: ConnectException =>
        Logger.error("Connection Error", e)
        InternalServerError("There is no internet connection. Please check your connection and retry.")
      case e: TimeoutException =>
        Logger.error("Timeout Error", e)
        InternalServerError("Timeout error, please try again.")
      case e: JsResultException =>
        Logger.error("Parsing Error", e)
        InternalServerError("News could not be parsed.")
      case NonFatal(n) =>
        Logger.error("Unexpected error", n)
        InternalServerError(n.getMessage)
    }(ec)
  }
}


