package controllers

import javax.inject.Inject
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.collection.mutable.Map

class Application @Inject() (theGuardianNewsFetcher: TheGuardianNewsFetcher) extends Controller {
  private val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  def index = Action.async { implicit request =>
    val totalNews: Int = request.getQueryString("totalNews").getOrElse("100").toInt
    val allNews: Future[Map[String, Int]] = theGuardianNewsFetcher.fetchNewsFor("turkey", totalNews)
    allNews.map(response => {
      val sortedWords: Seq[(String, Int)] = response.toSeq.sortWith(_._2 > _._2)
      Ok(sortedWords.toString())
    })(ec).recover{
      case NonFatal(n) =>
        InternalServerError(n.getMessage)
    }(ec)
  }
}


