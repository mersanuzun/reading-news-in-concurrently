package controllers

import scala.collection.immutable.HashMap
import scala.util.matching.Regex

/**
  * Created by mersanuzun on 12/12/16.
  */
class WordCounter {
  def calculateWordsfrequence(string: String): HashMap[String, Int] = {
    string.split(" ").foldLeft(HashMap[String, Int]()){
      (result, word) => {
        val prunedWord: String = pruneWord(word)
        result.get(prunedWord) match {
          case None => result + (prunedWord -> 1)
          case _ =>
            val wordFreq: Int = result(prunedWord)
            result + (prunedWord -> (wordFreq + 1))
        }
      }
    }
  }

  private def pruneWord(word: String) = {
    val pattern = new Regex("[a-zA-Z]+|[0-9]+[\\,\\.]?[0-9]+")
    pattern.findAllIn(word)
      .mkString
      .toLowerCase
  }
}
