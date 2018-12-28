import scala.annotation.tailrec
import scala.math._
import scala.collection.mutable.HashSet
import scala.collection.mutable._

  object i extends App {


    def countChange(money: Int, coins: List[Int]): Int =
    {
      if(money == 0)
        1
      else if(money > 0 && coins.nonEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else
        0
    }
//no negative input
    def pascal(c: Int, r: Int): Int = {
      if ((c == 0) || (r == c)) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }


    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def balanced(chars: List[Char], open: Int): Boolean =
        if (chars.isEmpty) open == 0
        else chars.head match {
          case '(' => balanced(chars.tail, open + 1)
          case ')' => open > 0 && balanced(chars.tail, open - 1)
          case _ => balanced(chars.tail, open)
        }

      balanced(chars, 0)
    }

    /*
println(balance(")()()()()".toList))
    println(balance("()(((())))".toList))
    println(balance("(GGGGGGGGGGGGGG)(A)".toList))

    println(pascal(2,4))

    println(pascal(0,0))
*/

    abstract class CodeTree
    case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
    case class Leaf(char: Char, weight: Int) extends CodeTree

    def weight(tree: CodeTree): Int = tree match {
      case Leaf(_, weight) => weight
      case Fork(left, right, _, _) => weight(left) + weight(right)
    }

    def numJewelsInStones(J: String, S: String): Int = {
      val theset = J.toSet
      S.filter(x => theset.contains(x)).size
    }

    def repeatedNTimes(A: Array[Int]): Int = {
      val s = A.size/2
      A.groupBy(identity).mapValues(_.size).filter(_._2 == s).head._1
    }
  }




//below this line copy paste code for functional sets
object FunSets {
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set={
    (x : Int) => x==elem
  }

  def union(s: Set, t: Set): Set = {
    (x:Int )  => s(x)||t(x)
  }

  def intersect(s: Set, t: Set): Set = {
    (x:Int) => s(x)&&t(x)
  }

  def diff(s: Set, t: Set): Set = {
    (x:Int) => s(x)&& !t(x)
  }

  def filter(s: Set, p: Int => Boolean): Set ={
    (x:Int) => s(x)&& p(x)
  }

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true
      else if (contains(s,a) && !p(a)) false
      else iter(a-1)
    }
    iter(bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = {
    !(forall ( s, (x:Int)=> !p(x)))
  }

  def map(s: Set, f: Int => Int): Set = {
    (x:Int) => exists(s, (a:Int)=> f(a) == x)
  }

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: Set) {
    println(toString(s))
  }
}