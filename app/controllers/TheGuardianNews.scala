package controllers

import scala.collection.mutable.{Map => MMap}

/**
  * Created by mersanuzun on 12/27/16.
  */
case class TheGuardianNews(url: String,
                           var fetchingTime: Option[Long] = None,
                           var content: Option[MMap[String, Int]] = None,
                           var status: Option[Int] = None,
                           var tryCount: Int = 3){
  def decreaseTryCount(): Unit = tryCount -= 1
}
