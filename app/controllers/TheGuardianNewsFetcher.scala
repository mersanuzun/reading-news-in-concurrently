package controllers

import java.net.ConnectException
import javax.inject.Inject
import com.fasterxml.jackson.core.JsonParseException
import com.google.inject.Singleton
import play.api.http.Status
import play.api.{Configuration, Logger}
import play.api.libs.json.{JsResultException, JsValue}
import scala.concurrent.duration._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.collection.mutable.{Map => MMap}
import scala.util.control.NonFatal
/**
  * Created by mersanuzun on 12/5/16.
  */
@Singleton
class TheGuardianNewsFetcher @Inject() (ws: WSClient, configuration: Configuration, wordCounter: WordCounter) {
  private val theGuardianUrl: String = configuration.getString("theGuardianNewsFetcher.url").get
  private val theGuardianApiKey: String = configuration.getString("theGuardianNewsFetcher.api_key").get
  private val defaultPageSize: Int = configuration.getInt("theGuardianNewsFetcher.pageSize").getOrElse(30)
  private val groupSize: Int = configuration.getInt("theGuardianNewsFetcher.groupSize").getOrElse(10)
  private val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  private val timeoutDuration: Duration = 5000.millis

  def fetchNewsFor(queryTerm: String, totalNewsAmount: Int): Future[MMap[String, Int]] = {
    val pageNumberAndSizes: IndexedSeq[(Int, Int)] = calculatePageNumbersAndSizes(totalNewsAmount)
    val request: WSRequest = ws.url(theGuardianUrl)
      .withHeaders("api-key" -> theGuardianApiKey)
      .withQueryString("q" -> queryTerm)

    pageNumberAndSizes.foldLeft(Future.successful(MMap.empty[String, Int])){
      (prevFutureContent: Future[MMap[String, Int]], page: (Int, Int)) => {
        prevFutureContent.flatMap(prevPagesContents => {
          fetchNewsLinksFromTheGuardian(page._1, page._2, request).flatMap(urls => {
            fetchNewsContentsFromTheGuardian(urls, prevPagesContents)
          })(ec)
        })(ec)
      }
    }
  }

  private def calculatePageNumbersAndSizes(totalNewsAmount: Int): IndexedSeq[(Int, Int)] = {
    var remainingNews: Int = totalNewsAmount
    (1 to calculateTotalPageNumber(totalNewsAmount)).map(page => {
      val pageSize: Int = if(remainingNews >= defaultPageSize) defaultPageSize
      else remainingNews
      remainingNews -= defaultPageSize
      (page, pageSize)
    })
  }

  private def calculateTotalPageNumber(totalNewsAmount: Int): Int = {
    Math.ceil(totalNewsAmount.toFloat / defaultPageSize).toInt
  }

  private def fetchNewsLinksFromTheGuardian(pageNumber: Int, pageSize: Int, request: WSRequest): Future[List[String]] = {
    val response: Future[WSResponse] = request.withQueryString("page" -> pageNumber.toString)
      .withRequestTimeout(5000.millis)
      .withQueryString("page-size" -> pageSize.toString)
      .get()

    def getUrlForError(): String = {
      request.uri + "&page=" + pageNumber + "&page-size=" + pageSize
    }

    response.map((r: WSResponse) => {
      if (r.status == Status.OK){
        parseUrls(r.json)
      }else{
        Logger.error("The guardian response is not OK with " + getUrlForError())
        List.empty[String]
      }
    })(ec).recover{
      case e: ConnectException => {
        Logger.error("The Guardian API could not be connected with " + getUrlForError(), e)
        List.empty[String]
        //throw new Exception("The Guardian could not be connected.")
      }
      case e: JsonParseException => {
        Logger.error("Response could not be convert to json.", e)
        List.empty[String]
      }
      case e: TimeoutException => {
        Logger.error("Timeout error was occurred with " + timeoutDuration + " duration for " + getUrlForError(), e)
        List.empty[String]
        //throw new Exception("Timeout error was occurred, please try again")
      }
      case NonFatal(n) => {
        Logger.error("Some error was occurred. " + n.getMessage, n)
        List.empty[String]
        //throw new Exception("Some error was occurred.")
      }
    }(ec)
  }

  private def parseUrls(responseJson: JsValue): List[String] = {
    try{
      (responseJson \ "response" \ "results").as[List[JsValue]].map(
        (result: JsValue) => {
          (result \ "apiUrl").as[String]
        })
    }catch {
      case e: JsResultException => {
        Logger.error("News urls could not be parsed", e)
        List.empty[String]
      }
    }
  }

  private def fetchNewsContentsFromTheGuardian(urls: List[String], prevPagesContents: MMap[String, Int]): Future[MMap[String, Int]] = {
    val groupedUrls: Iterator[List[String]] = urls.grouped(groupSize)
    groupedUrls.foldLeft(Future.successful(prevPagesContents)){
      (prevFuture: Future[MMap[String, Int]], nextUrls: List[String]) => {
        prevFuture.flatMap(prevContents => {
          fetchOneBlockNewsContents(nextUrls, prevContents).map(newsContents => {
            newsContents.foreach {
              case Right(content) =>
                prevContents ++= content.map {
                  case (k, v) => k -> (v + prevContents.getOrElse(k, 0))
                }
              case Left(url) => {

              }
            }
            prevContents
          })(ec)
        })(ec)
      }
    }
  }

  private def fetchOneBlockNewsContents(urls: List[String], prevContents: MMap[String, Int]): Future[List[Either[String, MMap[String, Int]]]] = {
    val futureContents: List[Future[Either[String, MMap[String, Int]]]] = urls.map(fetchOneNewsContent)
    Future.sequence(futureContents)(implicitly, ec)
  }

  private def fetchOneNewsContent(url: String): Future[Either[String, MMap[String, Int]]] = {
    try{
      val response: Future[WSResponse] = ws.url(url)
        .withHeaders("api-key" -> theGuardianApiKey)
        .withQueryString("show-fields" -> "all")
        .withRequestTimeout(timeoutDuration)
        .get

      response.map(r => {
        if (r.status == Status.OK){
          parseOneNewsContent(r.json) match {
            case Some(s) => Right(s)
            case None => Left(url)
          }
        }else {
          val errorMessage: String = (r.json \ "message").as[String]
          Logger.error(errorMessage)
          Left(url)
          //throw new Exception("Response status is " + r.status + ".\n" + errorMessage)
        }
      })(ec).recover{
        case e: JsonParseException=> {
          Logger.error(url + " news url could not converted to json.", e)
          Left(url)
        }
        case e: ConnectException => {
          Logger.error("The Guardian API could not be connected.", e)
          Left(url)
        }
        case e: TimeoutException => {
          Logger.warn("Timeout error was occurred with " + timeoutDuration + " duration.", e)
          Left(url)
        }
        case NonFatal(n) => {
          Logger.error("Some error was occurred. " + n.getMessage, n)
          Left(url)
        }
      }(ec)
    }catch{
      case NonFatal(n) => {
        Logger.error(url + " could not be fetched.")
        Future.successful(Left(url))
      }
    }
  }

  private def parseOneNewsContent(responseJson: JsValue): Option[MMap[String, Int]] = {
    try{
      val content: String = (responseJson \ "response" \ "content" \ "fields" \ "bodyText").as[String]
      Some(wordCounter.calculateWordFrequency(content))
    }catch{
      case e: JsResultException => {
        Logger.error(responseJson + " could not be parsed.", e)
        None
      }
    }
  }

  /*private def addNewsFrequency(prevContents: MMap[String, Int], newsContent: MMap[String, Int]) = {
    prevContents ++= newsContent.map {
      case (k, v) => k -> (v + prevContents.getOrElse(k, 0))
    }
  }*/
}