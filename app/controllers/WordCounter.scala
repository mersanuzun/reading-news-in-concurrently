package controllers

import javax.inject.Singleton

import scala.util.matching.Regex
import scala.collection.mutable.Map

/**
  * Created by mersanuzun on 12/12/16.
  */
@Singleton
class WordCounter {
  private val pattern = new Regex("[a-zA-Z]+[\\-\\'\\’\\`]?[a-zA-Z]+|[0-9]+[\\,\\.]?[0-9]*")

  def calculateWordFrequency(string: String): Map[String, Int] = {
    string.split("\\s").foldLeft(Map.empty[String, Int]){
      (wordsFrequency, word) => {
        pruneWord(word).foreach(prunedWord => {
          val lowerWord: String = prunedWord.toLowerCase
          if (lowerWord.nonEmpty){
            wordsFrequency.get(lowerWord) match {
              case None => wordsFrequency += (lowerWord -> 1)
              case Some(s) => wordsFrequency += (lowerWord -> (s + 1))
            }
          }
        })
        wordsFrequency
      }
    }
  }

  //if - comes to pruneword method return empty string
  //prune words from Deneme. "dEneme, deNeme? denEme: deneMe's didn't
  //ersan,uzun => [ersan, uzun]
  private def pruneWord(word: String): List[String] = {
    pattern.findAllIn(word)
        .toList
  }
}


/*
  pattern içerde tanımladığım da -> 1227milis
  pattern dışardayken -> 460milis
 */

