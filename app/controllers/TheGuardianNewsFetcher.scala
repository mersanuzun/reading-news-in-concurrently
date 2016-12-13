package controllers

import java.util.concurrent.Executors
import javax.inject.Inject
import play.api.libs.json.JsValue
import scala.concurrent.duration._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by mersanuzun on 12/5/16.
  */
class TheGuardianNewsFetcher @Inject() (ws: WSClient, configuration: play.api.Configuration) {
  private val theGuardianUrl: String = configuration.underlying.getString("theGuardianNewsFetcher.url")
  private val theGuardianApiKey: String = configuration.underlying.getString("theGuardianNewsFetcher.api_key")
  private val pageSize: Int = configuration.underlying.getInt("theGuardianNewsFetcher.pageSize")
  private implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(15))
  private val timeoutDuration: Duration = 5000.millis

  def fetchNewsFor(queryTerm: String, newsLimit: Int): Future[List[String]] = {
    val totalPageSize: Int = Math.ceil(newsLimit.toFloat / pageSize).toInt
    val request: WSRequest = ws.url(theGuardianUrl)
      .withHeaders("api-key" -> theGuardianApiKey)
      .withQueryString("q" -> queryTerm)
      .withQueryString("page-size" -> pageSize.toString)

    (1 to totalPageSize).foldLeft(Future(List.empty[String])){
      (prevFuture, page) => {
        for{
          prevResults <- prevFuture
          urls <- fetchNewsLinks(page, request)
          contents <- fetchOnePageNewsContents(urls)
        } yield prevResults ++ contents
      }
    }
  }

  private def fetchNewsLinks(page: Int, request: WSRequest): Future[List[String]] = {
    val response: Future[WSResponse] = request.withQueryString("page" -> page.toString)
      .withRequestTimeout(timeoutDuration)
      .get()
    parseUrls(response)
  }

  private def parseUrls(response: Future[WSResponse]): Future[List[String]] = {
    response.map(r => {
      if (r.status >= 200 && r.status <= 400){
        (r.json \ "response" \ "results").as[List[JsValue]].map{
          jsonResponse => (jsonResponse \ "apiUrl").as[String]
        }
      }else{
        val errorMessage: String = (r.json \ "message").as[String]
        throw new Exception(errorMessage)
      }
    })
  }

  private def fetchOnePageNewsContents(urls: List[String]): Future[List[String]] = {
    makeGroupNews(urls, 10).foldLeft(Future(List.empty[String])){
      (prevFuture, nextUrls) => {
        for{
          prevResults <- prevFuture
          next <- fetchABlockOfNewsContents(nextUrls)
        }yield prevResults ++ next
      }
    }
  }

  private def makeGroupNews(urlList: List[String], groupSize: Int): List[List[String]] = {
    urlList.grouped(groupSize).toList
  }

  private def fetchABlockOfNewsContents(newsList: List[String]): Future[List[String]] = {
    Future.sequence(newsList.map(fetchANewsContent))
  }

  private def fetchANewsContent(url: String): Future[String] = {
    val response: Future[WSResponse] = ws.url(url)
      .withHeaders("api-key" -> theGuardianApiKey)
      .withQueryString("show-fields" -> "all")
      .withRequestTimeout(timeoutDuration)
      .get
    parseANewsContent(response)
  }

  private def parseANewsContent(response: Future[WSResponse]): Future[String] = {
    response.map(r => {
      if (r.status >= 200 && r.status < 400){
        (r.json \ "response" \ "content" \ "fields" \ "bodyText").as[String]
      }else{
        val errorMessage: String = (r.json \ "message").as[String]
        throw new Exception(errorMessage)
      }
    })
  }
}