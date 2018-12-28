import scala.math._
import scala.collection.mutable.HashSet

import scala.collection.mutable._

  object i extends App {

    def infiniteList(n:Int):Stream[Int] = {
      n #:: infiniteList(n)
    }

  //  infiniteList(25).foreach( println _ )

    //dfs stuff
    var g:Map[Int,List[Int]] = Map()


    g(0) = List(1,2,3)
    g(1) = List(0,4)
    g(2) = List(0)
    g(3) = List(0,4)
    g(4) = List(0,3)
    //bipartite graph

    def DFS(start: Int): List[Int] = {

      def DFS0(v: Int, visited: List[Int]): List[Int] = {
        if (visited.contains(v))
          visited
        else {
          val neighbours:List[Int] = g(v) filterNot visited.contains
          neighbours.foldLeft(v :: visited)((b,a) => DFS0(a,b))
        }
      }
      DFS0(start,List()).reverse
    }

println(DFS(1))
    //huffman encoding

    //put freq counts and chars into pq
    //while not empty, combine 2 lowest freq counts and into one node with parent
    //decode-left is 0, right is 1

    private abstract sealed class Tree[A] {
      val freq: Int
      def toCode: List[(A, String)] = toCodePrefixed("")
      def toCodePrefixed(prefix: String): List[(A, String)]
    }
    private final case class InternalNode[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
      val freq: Int = left.freq + right.freq
      def toCodePrefixed(prefix: String): List[(A, String)] =
        left.toCodePrefixed(prefix + "0") ::: right.toCodePrefixed(prefix + "1")
    }
    private final case class LeafNode[A](element: A, freq: Int) extends Tree[A] {
      def toCodePrefixed(prefix: String): List[(A, String)] = List((element, prefix))
    }

    def huffman[A](ls: List[(A, Int)]): List[(A, String)] =
    {
      import collection.immutable.Queue
      def dequeueSmallest(q1: Queue[Tree[A]], q2: Queue[Tree[A]]) = {
        // This ordering chooses q1 in case of ties, which helps minimize tree
        // depth.
        if (q2.isEmpty) (q1.front, q1.dequeue._2, q2)
        else if (q1.isEmpty || q2.front.freq < q1.front.freq) (q2.front, q1, q2.dequeue._2)
        else (q1.front, q1.dequeue._2, q2)
      }
      def huffmanR(q1: Queue[Tree[A]], q2: Queue[Tree[A]]): List[(A, String)] = {
        if (q1.length + q2.length == 1)
          (if (q1.isEmpty) q2.front else q1.front).toCode
        else
        {
          val (v1, q3, q4) = dequeueSmallest(q1, q2)
          val (v2, q5, q6) = dequeueSmallest(q3, q4)
          huffmanR(q5, q6.enqueue(InternalNode(v1, v2)))
        }
      }

      huffmanR(Queue.empty.enqueue(ls.sortWith(_._2 < _._2) map { e => LeafNode(e._1, e._2) }),
        Queue.empty)
    }




  }


