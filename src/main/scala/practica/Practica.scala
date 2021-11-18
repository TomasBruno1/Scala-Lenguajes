package practica

import scala.concurrent.Future
import scala.io.Source
import scala.math.abs
import scala.util.{Failure, Success}

case class Person(name: String, age: Int, children: List[Person] = Nil):

  def descendants: List[Person] = this :: children.flatMap(_.descendants)

  def adultsAndMinors: (List[Person], List[Person]) = descendants.partition(_.age >= 18)

  def withoutChildren: List[Person] = descendants.filter(_.children.isEmpty)

  def withChildren: List[Person] = descendants.filter(_.children.nonEmpty)

  def childrenByAge: List[Person] = children.sortBy(_.age)

  def consecutiveChildren: List[(Person, Person)] = childrenByAge.zip(childrenByAge.tail)

  def twinChildren: List[(Person, Person)] = consecutiveChildren.filter((a, b) => a.age == b.age)

  def twins = withChildren.flatMap(_.twinChildren)

  def fourYearDifferenceChildren = consecutiveChildren.filter((a,b) => b.age - a.age > 4) // this does not work

  def fourYearDifferenceBrothers = withChildren.flatMap(_.fourYearDifferenceChildren)

  def fourYearAverageChildren = children.foldLeft(0)(_ + _.age) / children.size

  def fourYearAverage = withChildren.filter(_.fourYearAverageChildren == 4)

  def childrenWithParentOlderThan(limit: Int): List[Person] = withChildren.filter(_.age > limit).flatMap(_.children)

  def grandChildren = children.flatMap(_.children)

  def allGrandChildren = withChildren.flatMap(_.grandChildren)


def phrases(text: String, phraseLength: Int = 3): Seq[List[String]] =

  // Sentences are the text separated by a dot (.)
  val sentences: List[String] = text
    .split('.')
    .toList

  val phrasesList = sentences.flatMap { sentence =>

    // Split each sentence word by word
    val words = sentence.split(' ')
      .toList

    // Group consecutive words as phrases
    words.sliding(phraseLength)
  }

  // Sliding can return phrases shorter than specified length
  phrasesList.filter(_.length == phraseLength)


def topPhrases(text: String): List[(String, Int)] =
  phrases(text).groupBy(e => e).map {
    case (phrase, phraseList) =>
      phrase.reduce(_ ++ " " ++ _) -> phraseList.size
  }.toList.sortBy(-_._2)

def mostFreqWord(words: Seq[String]): String = {
  val (topWord, wordCount) = words.groupBy(e => e).map {
    case (word, wordList) =>
      word -> wordList.length
  }.toList.maxBy(_._2)

  topWord
}

def nextWordMap(content: String): Map[List[String], String] = {

  val allPhrases: Seq[List[String]] = phrases(content)

  // Group 3-word phrases by the first two words (_.take(2))
  allPhrases
    .groupBy(_.take(2)) // all phrases have 3 words, but we take first two words to group them
    .map {
      case (phraseHead, phraseList) =>
        // phraseHead are a List of two words
        // phraseList is a list of 3-words phrases, we only need the last word
        val lastWords: Seq[String] = phraseList.map(_.last)

        // from all the 'lastWords', select the most frequent
        val word = mostFreqWord(lastWords)

        phraseHead -> word
    }
}

case class LocalizedString(location: List[String], content: String):
  def +(a: LocalizedString): LocalizedString = LocalizedString((location ++ a.location).distinct, content ++ a.content)

  def map(f: String => String) = LocalizedString(location, f(content))

object LocalizedString:
  given stringToLocalized: Conversion[String, LocalizedString] = s => LocalizedString(List(), s)

object Retry:
  private class RetryException extends RuntimeException
  private val e = new RetryException

  def retry(cond: Boolean): Unit =
    if(cond) throw e

  def retryable(op: => Unit): Unit =
    var cont = true

    while (cont)
      try
        cont = false
        op
      catch
        case _: RetryException =>
          cont = true

import concurrent.ExecutionContext.Implicits.global
def downloadAsync(url: String): Future[String] =
  Future {
    val source = Source.fromURL(url)
    val text = source.mkString
    source.close()
    println(s"Downloaded $url")
    text
  }

def compareUrlContent(url1: String, url2: String): Future[Boolean] =
  val a = downloadAsync(url1)
  val b = downloadAsync(url2)
  val l1: Future[Int] = a.map(_.length)
  val l2: Future[Int] = b.map(_.length)

  val result: Future[Boolean] = for {
    v1 <- l1
    v2 <- l2
  } yield v1 == v2

  result

def getRows(csv: String): List[List[String]] = csv.split('\n').flatMap(_.split(',')).toList.sliding(4, 4).toList

def tempSum(rows: List[List[String]]) = rows.foldLeft(0)(_ + _.last.toInt)

def mostBrusqueTemperatureChange(csv: String) =

  val rows = getRows(csv)

  rows.zip(rows.tail).maxBy((a, b) => abs(a.last.toInt - b.last.toInt))

def monthAvg(csv: String) =

  val rows = getRows(csv)

  rows.groupBy(_(1)).map { // tmb _.tail.head
    case (month, rows) =>
      month -> tempSum(rows) / rows.size
  }.toList.maxBy(_._2)

def threeMostHotConsecutive(csv: String) =

  val rows = getRows(csv)

  rows.sliding(3).maxBy(tempSum(_)/3)

object LogIfSlow:
  private class SlowException extends RuntimeException
  private val e = new SlowException

  def log(cond: Boolean): Unit =
    if(cond) throw e

  def logIfSlow(op: => Unit): Unit =
    var now = System.currentTimeMillis()

    try
      op
      log(System.currentTimeMillis() - now > 1000)
    catch
      case _: SlowException =>
        println("Slow code detected")

@main def main = {
//  import scala.language.implicitConversions
//  import LocalizedString.given
//
//  val a = LocalizedString(List("es"), "Hola")
//  val b = LocalizedString(List("en"), "World")
//
//  println(a + b)
//
//  println(a.map(_.toUpperCase))
//
//  println(a + " " + b)
//
//  import Retry._
//
//  var i = 0
//  retryable {
//    i = i + 1
//    println("i=" + i)
//    retry (i < 10)
//  }

//  val result = compareUrlContent("https://scala-lang.org", "https://scala-lang.org")
//
//  result.onComplete {
//    case Success(v) => println(v)
//    case Failure(e) => println("error")
//  }
//
//  println("Waiting...")
//  System.in.read()

//  val csv = "2015,03,05,20\n2015,03,06,28\n2015,03,07,31\n2015,03,08,41\n2015,03,09,40\n" +
//    "2015,04,05,30\n2015,04,06,31\n2015,04,07,29\n2015,04,08,35\n2015,04,09,40"
//
//  println(mostBrusqueTemperatureChange(csv))
//  println(monthAvg(csv))
//  println(threeMostHotConsecutive(csv))

//  import LogIfSlow._
//
//  println("1st")
//  logIfSlow {
//    1 + 1
//    println("very fast")
//  }
//  println("2nd")
//  logIfSlow {
//    Thread.sleep(2000)
//    1 + 1
//    println("that was slow!")
//  }


}



