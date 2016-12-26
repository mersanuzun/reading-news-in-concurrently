package controllers

import java.net.ConnectException
import javax.inject.Inject
import com.google.inject.Singleton
import play.api.{Configuration, Logger}
import play.api.libs.json.{JsResultException, JsValue}
import scala.concurrent.duration._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.collection.mutable.Map
import scala.util.control.NonFatal
/**
  * Created by mersanuzun on 12/5/16.
  */
@Singleton
class TheGuardianNewsFetcher @Inject() (ws: WSClient, configuration: Configuration, wordCounter: WordCounter) {
  private val theGuardianUrl: String = configuration.underlying.getString("theGuardianNewsFetcher.url")
  private val theGuardianApiKey: String = configuration.underlying.getString("theGuardianNewsFetcher.api_key")
  private val defaultPageSize: Int = configuration.underlying.getInt("theGuardianNewsFetcher.pageSize")
  private val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  private val timeoutDuration: Duration = 5000.millis
  private val OK: Int = 200

  def fetchNewsFor(queryTerm: String, totalNewsAmount: Int): Future[Map[String, Int]] = {
    val pageSizes: List[Int] = calculatePageSizes(totalNewsAmount)
    val request: WSRequest = ws.url(theGuardianUrl)
      .withHeaders("api-key" -> theGuardianApiKey)
      .withQueryString("q" -> queryTerm)

    (1 to totalPageSize(totalNewsAmount)).foldLeft(Future.successful(Map.empty[String, Int])){
      (prevFuture, page) => {
        prevFuture.flatMap(prevPagesContents => {
          fetchAPageOfNewsLinks(page, pageSizes(page - 1), request).flatMap(urls => {
            fetchOnePageNewsContents(urls, prevPagesContents)
          })(ec)
        })(ec)
      }
    }
  }

  private def calculatePageSizes(totalNewsAmount: Int): List[Int] = {
    var remainingNews: Int = totalNewsAmount
    (1 to totalPageSize(totalNewsAmount)).map(_ => {
      val pageSize: Int = if(remainingNews > 0 && remainingNews >= defaultPageSize) defaultPageSize
      else remainingNews
      remainingNews -= defaultPageSize.toInt
      pageSize
    }).toList
  }

  private def totalPageSize(totalNewsAmount: Int): Int = {
    Math.ceil(totalNewsAmount.toFloat / defaultPageSize).toInt
  }

  private def fetchAPageOfNewsLinks(page: Int, pageSize: Int, request: WSRequest): Future[List[String]] = {
    val response: Future[WSResponse] = request.withQueryString("page" -> page.toString)
      .withRequestTimeout(timeoutDuration)
      .withQueryString("page-size" -> pageSize.toString)
      .get()
    parseUrls(response)
  }

  private def parseUrls(response: Future[WSResponse]): Future[List[String]] = {
    response.map(r => {
      if (r.status == OK){
        (r.json \ "response" \ "results").as[List[JsValue]].map{
          jsonResponse => (jsonResponse \ "apiUrl").as[String]
        }
      }else{
        val errorMessage: String = (r.json \ "message").as[String]
        Logger.error(errorMessage + " with " + theGuardianApiKey + " The Guardian API key. Response status is " + r.status)
        throw new Exception(errorMessage + " for The Guardian API.")
      }
    })(ec).recover{
      case e: ConnectException => {
        Logger.error("There is no internet connection.", e)
        throw new Exception("There is no internet connection. Please check your connection and retry.")
      }
      case e: JsResultException => {
        Logger.error("News urls could not be parsed for " + theGuardianApiKey + " The Guardian API key.", e)
        throw new Exception("News urls could not be parsed.")
      }
      case e: TimeoutException => {
        Logger.warn("Timeout error", e)
        throw new Exception("Timeout error was occurred, please try again")
      }
      case e: Exception => {
        throw e
      }
      case NonFatal(n) => {
        Logger.error("Unexpected error.", n)
        throw new Exception("Unexpected error was occurred.")
      }
    }(ec)
  }

  private def fetchOnePageNewsContents(urls: List[String], prevPagesContents: Map[String, Int]): Future[Map[String, Int]] = {
    makeGroupNews(urls, 10).foldLeft(Future.successful(prevPagesContents)){
      (prevFuture, nextUrls) => {
        prevFuture.flatMap(prevContents => {
          fetchABlockOfNewsContents(nextUrls).map(newsContents => {
            newsContents.foreach(newsContent => {
              prevContents ++= newsContent.map{
                case (k, v) => k -> (v + prevContents.getOrElse(k, 0))
              }
            })
            prevContents
          })(ec)
        })(ec)
      }
    }
  }

  private def makeGroupNews(urlList: List[String], groupSize: Int): List[List[String]] = {
    urlList.grouped(groupSize).toList
  }

  private def fetchABlockOfNewsContents(newsList: List[String]): Future[List[Map[String, Int]]] = {
    Future.sequence(newsList.map(fetchANewsContent))(implicitly, ec)
  }

  private def fetchANewsContent(url: String): Future[Map[String, Int]] = {
    val response: Future[WSResponse] = ws.url(url)
      .withHeaders("api-key" -> theGuardianApiKey)
      .withQueryString("show-fields" -> "all")
      .withRequestTimeout(timeoutDuration)
      .get
    parseANewsContent(response, url)
  }

  private def parseANewsContent(response: Future[WSResponse], url: String): Future[Map[String, Int]] = {
    response.map(r => {
      if (r.status == OK){
        val content: String = (r.json \ "response" \ "content" \ "fields" \ "bodyText").as[String]
        countWords(content)
      }else {
        val errorMessage: String = (r.json \ "message").as[String]
        throw new Exception("Response status is " + r.status + ".\n" + errorMessage)
      }
    })(ec).recover{
      case e: JsResultException => {
        Logger.error(url + " could not parsed. ", e)
        //throw new Exception(url + " could not parsed.")
        Map.empty[String, Int]
      }
      case NonFatal(e) => {
        Logger.error(e.getMessage, e)
        //throw e
        Map.empty[String, Int]
      }
    }(ec)
  }

  private def countWords(newsContent: String): Map[String, Int] = {
      wordCounter.calculateWordFrequency(newsContent)
  }
}