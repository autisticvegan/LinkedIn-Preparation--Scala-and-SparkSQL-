import scala.annotation.tailrec
import scala.math._
import scala.collection.mutable.HashSet

//copy and paste

import scala.io.Source

  object i extends App {


    val test = List(1,2,3,4)
    test.foldLeft(0)(_ + _)
  //  println(test)

    val filename = "linuxwords.txt"
  //  for (line <- Source.fromFile(filename).getLines) {
   //   println(line)
   // }


    def wordOccurrences(w: String): List[(Char,Int)] =
      w.toList.groupBy ( c => c.toLower ).map{ case (c, cl) => (c, cl.size) }.toList.sorted

    val dictionary = Source.fromFile(filename).getLines.toList

    lazy val dictionaryByOccurrences: Map[List[(Char,Int)], List[String]] =
      dictionary.groupBy( w => wordOccurrences(w) )

    /** Returns all the anagrams of a given word. */
    def wordAnagrams(word: String): List[String] =
      dictionaryByOccurrences(wordOccurrences(word))



    def sentenceOccurrences(s: List[String]): List[(Char,Int)] =
      wordOccurrences(s.foldLeft("")(_ + _))

    def combinations(occurrences: List[(Char,Int)]): List[List[(Char,Int)]] =
    {
      if (occurrences isEmpty)
        List(Nil)
      else {
        val children: List[(Char, Int)] =
          for {
            occ <- occurrences
            i <- 1 to occ._2
          } yield (occ._1, i)

        val subs : List[List[(Char,Int)]] =
          children.map(c => subtract(occurrences, List(c)))

        occurrences :: subs.flatMap((sub : List[(Char,Int)]) => combinations(sub)).toSet.toList
      }
    }

    def subtract(x: List[(Char,Int)], y: List[(Char,Int)]): List[(Char,Int)] =
    {
      if (x isEmpty)
        Nil
      else if (y isEmpty)
        x
      else if (x.head._1 == y.head._1)
        if (x.head._2 == y.head._2)
          subtract(x.tail, y.tail)
        else
          (x.head._1, x.head._2 - y.head._2) :: subtract(x.tail, y.tail)
      else
        x.head :: subtract(x.tail, y)
    }

    def Sentence_Anagrams_Memo(list_of_words : List[String]) : List[List[String]] =
    {
      val cache: scala.collection.mutable.Map[List[(Char,Int)], List[List[String]]] = scala.collection.mutable.Map()

      def Outer_Loop(occurences : List[(Char,Int)]) : List[List[String]] =
      {
        cache.getOrElseUpdate(occurences, Inner_Loop(occurences))
      }

      def Inner_Loop(occurrences : List[(Char,Int)]) : List[List[String]] =
      {
        if (occurrences.isEmpty)
          List(Nil)
        else
          for {
            subset <- combinations(occurrences)
            if (dictionaryByOccurrences.contains(subset))
            word <- dictionaryByOccurrences(subset)
            list_of_words <- Outer_Loop(subtract(occurrences, subset))
          } yield word :: list_of_words
      }

      Outer_Loop(sentenceOccurrences(list_of_words))
    }

    val list = List[String]("evil")

    println(Sentence_Anagrams_Memo(list))
  }