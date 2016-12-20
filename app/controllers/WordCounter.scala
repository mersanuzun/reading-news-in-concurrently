package controllers

import scala.collection.immutable.HashMap
import scala.util.matching.Regex

/**
  * Created by mersanuzun on 12/12/16.
  */
class WordCounter {
  private val pattern = new Regex("[a-zA-Z\\-\\'\\’\\`]|[0-9]+[\\,\\.]?[0-9]*")

  /*def calculatePrunedWords(prunedWords: List[String]) = {
    prunedWords.foldLeft(Map.empty[String, Int]){
      (prunedWordsFrequency, prunedWord) => {
        prunedWordsFrequency.get(prunedWord) match {
          case None => prunedWordsFrequency + (prunedWord -> 1)
          case _ =>
            val wordFreq: Int = prunedWordsFrequency(prunedWord)
            prunedWordsFrequency + (prunedWord -> (wordFreq + 1))
        }
      }
    }
  }*/

  def calculateWordFrequency(string: String): Map[String, Int] = {
    string.split("\\s").foldLeft(Map.empty[String, Int]){
      (wordsFrequency, word) => {
        //val s: Map[String, Int] = calculatePrunedWords(pruneWord(word))
        val prunedWord: String = pruneWord(word)
        wordsFrequency.get(prunedWord) match {
          case None => wordsFrequency + (prunedWord -> 1)
          case _ =>
            val wordFreq: Int = wordsFrequency(prunedWord)
            wordsFrequency + (prunedWord -> (wordFreq + 1))
        }
      }
    }
  }

  //prune words from Deneme. dEneme, deNeme? denEme: deneMe's didn't
  private def pruneWord(word: String): String = {
    pattern.findAllIn(word)
      //.map(_.toUpperCase)
      //.toList
      .mkString
      .toLowerCase
  }

}


/*
  pattern içerde tanımladığım da -> 1227milis
  pattern dışardayken -> 460milis
 */

