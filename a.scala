import scala.annotation.tailrec

object a extends App {

  def LastElement[A](list: List[A]): A =
  {
    list.last
  }

  def LastElementEX[A](list: List[A]): A =
  {
    list match {
      case head :: Nil => head
      case _ :: tail => LastElementEX(tail)
      case _ => throw new NoSuchElementException
    }
  }

  def SecondLastElement[A](list: List[A]): A =
  {
    list match
    {
      case head :: _ :: Nil => head
      case _ :: tail        => SecondLastElement(tail)
      case _                => throw new NoSuchElementException
    }
//second way
    if(list.isEmpty) throw new NoSuchElementException
    else list.init.last

  }

  def pack[A](list : List[A]) : List[List[A]] =
  {
    if(list.isEmpty) List(List())
    else
      {
        val (packed, next) =  list span { _ == list.head }
        if(next == Nil) List(packed)
        else packed :: pack(next)
      }
  }



  def Generate_Combinations[A](list: List[A], number_of_objects: Int) : List[List[A]] =
  {
    list.combinations(number_of_objects).toList
  }

  def encode[A](list : List[A]) : List[(Int, A)] =
  {
    pack(list) map { x => (x.length, x.head)}
  }

  def List_Sort[A](list: List[List[A]]) : List[List[A]] =
  {
    list.sortBy(_.length)

  }

  def Lists_Sort_By_Frequency[A](list : List[List[A]]) : List[List[A]] =
  {
    val t = List_Sort(list)
    val tt = t.groupBy(_.length)
    val ttt = tt.values.toList
    val tttt = ttt.sortBy(_.length)
    tttt.flatten

  }

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }
  
  def Group_Elements_Into_Disjoint_Subsets[A] (group_sizes : List[Int], list : List[A]) : List[List[List[A]]] =
  {
    group_sizes match
    {
      case Nil => List(Nil)
      case x :: group_sizes => Generate_Combinations(list,x) flatMap { y =>
        Group_Elements_Into_Disjoint_Subsets(group_sizes, list diff(y)) map {y :: _} }
    }
  }

  val lsofls = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
  val ls2 = List("gas","gas", "gas", "powered", "toilet", "toilet", "touch", "poopoo")
  val ls = List("whyteepee","abc","coc","sus","joj","hohsis","did whatever it took", "piece of tape", "on the house")
val helpmejesus = (List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  //println(LastElementEX(ls))
 // println(SecondLastElement(ls))
//println(pack(ls2))
//println(encode(ls2))

//  println(Lists_Sort_By_Frequency(lsofls))

 //   println(Group_Elements_Into_Disjoint_Subsets(helpmejesus._1, helpmejesus._2))
}
