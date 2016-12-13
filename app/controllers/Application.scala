package controllers

import java.net.ConnectException
import java.util.concurrent.Executors
import javax.inject.Inject
import play.Logger
import play.api.libs.json.JsResultException
import play.api.mvc._
import scala.collection.immutable.HashMap
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.control.NonFatal

class Application @Inject() (theGuardianNewsFetcher: TheGuardianNewsFetcher, wordCounter: WordCounter) extends Controller {
  private implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

  def index = Action.async {
    val allNews: Future[List[String]] = theGuardianNewsFetcher.fetchNewsFor("turkey", 90)
    allNews.map(response => {
      val wordsFreq: HashMap[String, Int] = wordCounter.calculateWordsfrequence(response.mkString)
      val sortedWords: Seq[(String, Int)] = wordsFreq.toSeq.sortWith(_._2 > _._2)
      Ok(sortedWords.toString)
    }).recover{
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
        Logger.error("Some Error", n)
        InternalServerError(n.getMessage)
    }
  }

}