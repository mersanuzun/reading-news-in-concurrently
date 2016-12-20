package controllers

import javax.inject.Inject
import play.api.libs.json.JsValue
import scala.concurrent.duration._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by mersanuzun on 12/5/16.
  */
class TheGuardianNewsFetcher @Inject() (ws: WSClient, configuration: play.api.Configuration, wordCounter: WordCounter) {
  private val theGuardianUrl: String = configuration.underlying.getString("theGuardianNewsFetcher.url")
  private val theGuardianApiKey: String = configuration.underlying.getString("theGuardianNewsFetcher.api_key")
  private val pageSize: Int = configuration.underlying.getInt("theGuardianNewsFetcher.pageSize")
  private val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  private val timeoutDuration: Duration = 5000.millis

  def fetchNewsFor(queryTerm: String, newsLimit: Int): Future[Map[String, Int]] = {
    val totalPageSize: Int = Math.ceil(newsLimit.toFloat / pageSize).toInt
    val request: WSRequest = ws.url(theGuardianUrl)
      .withHeaders("api-key" -> theGuardianApiKey)
      .withQueryString("q" -> queryTerm)
      .withQueryString("page-size" -> pageSize.toString)

    (1 to totalPageSize).foldLeft(Future(Map.empty[String, Int])(ec)){
      (prevFuture, page) => {
        prevFuture.flatMap(prevPagesContents => {
          println("page -> " + page)
          fetchAPageOfNewsLinks(page, request).flatMap(urls => {
            fetchOnePageNewsContents(urls).map(contents => {
              concatMaps(prevPagesContents, contents)
            })(ec)
          })(ec)
        })(ec)
      }
    }
  }

  private def fetchAPageOfNewsLinks(page: Int, request: WSRequest): Future[List[String]] = {
    val response: Future[WSResponse] = request.withQueryString("page" -> page.toString)
      .withRequestTimeout(timeoutDuration)
      .get()
    parseUrls(response)
  }

  private def parseUrls(response: Future[WSResponse]): Future[List[String]] = {
    response.map(r => {
      if (r.status == 200){
        (r.json \ "response" \ "results").as[List[JsValue]].map{
          jsonResponse => (jsonResponse \ "apiUrl").as[String]
        }
      }else{
        val errorMessage: String = (r.json \ "message").as[String]
        throw new Exception(errorMessage)
      }
    })(ec)
  }

  private def fetchOnePageNewsContents(urls: List[String]): Future[Map[String, Int]] = {
    makeGroupNews(urls, 10).foldLeft(Future(Map.empty[String, Int])(ec)){
      (prevFuture, nextUrls) => {
        prevFuture.flatMap(prevContents => {
          fetchABlockOfNewsContents(nextUrls).map(newContents => {
            concatMaps(prevContents, newContents)
          })(ec)
        })(ec)
      }
    }
  }

  private def makeGroupNews(urlList: List[String], groupSize: Int): List[List[String]] = {
    urlList.grouped(groupSize).toList
  }

  private def fetchABlockOfNewsContents(newsList: List[String]): Future[Map[String, Int]] = {
    newsList.map(fetchANewsContent).foldLeft(Future(Map.empty[String, Int])(ec)){
      (prevFuture, next) => {
        prevFuture.flatMap(prevContents => {
          val s = System.currentTimeMillis()
          next.map(newContent => {
            concatMaps(prevContents, newContent)
          })(ec)
        })(ec)
      }
    }
  }

  private def fetchANewsContent(url: String): Future[Map[String, Int]] = {
    val response: Future[WSResponse] = ws.url(url)
      .withHeaders("api-key" -> theGuardianApiKey)
      .withQueryString("show-fields" -> "all")
      .withRequestTimeout(timeoutDuration)
      .get
    val parsedContent: Future[String] = parseANewsContent(response)
    countWords(parsedContent)
  }

  private def parseANewsContent(response: Future[WSResponse]): Future[String] = {
    response.map(r => {
      if (r.status == 200){
        (r.json \ "response" \ "content" \ "fields" \ "bodyText").as[String]
      }else{
        val errorMessage: String = (r.json \ "message").as[String]
        throw new Exception(errorMessage)
      }
    })(ec)
  }

  private def countWords(newsContent: Future[String]): Future[Map[String, Int]] = {
    newsContent.map(content => {
      wordCounter.calculateWordFrequency(content)
    })(ec)
  }

  private def concatMaps(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    map1 ++ map2.map{
      case (k,v) =>
        k -> (v + map1.getOrElse(k, 0))
    }
  }
}