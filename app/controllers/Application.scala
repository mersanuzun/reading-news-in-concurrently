package controllers

import javax.inject.Inject

import play.api.Logger
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.collection.mutable.{Map => MMap}

class Application @Inject() (theGuardianNewsFetcher: TheGuardianNewsFetcher) extends Controller {
  private val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  def index = Action.async { implicit request =>
    try{
      val totalNews: Int = request.getQueryString("totalNews").getOrElse("100").toInt
      val queryTerm: String = request.getQueryString("queryTerm").getOrElse("turkey")
      val allNews: Future[MMap[String, Int]] = theGuardianNewsFetcher.fetchNewsFor(queryTerm, totalNews)
      allNews.map(response => {
        val sortedWords: Seq[(String, Int)] = response.toSeq.sortWith(_._2 > _._2)
        Ok(sortedWords.toString)
      })(ec).recover{
        case NonFatal(n) =>
          InternalServerError(n.getMessage)
      }(ec)
    }catch{
      case e: NumberFormatException =>
        Logger.warn("Client sent non-numeric totalNews.", e)
        Future.successful(InternalServerError("Total news amount should be numeric."))
    }
  }
}


