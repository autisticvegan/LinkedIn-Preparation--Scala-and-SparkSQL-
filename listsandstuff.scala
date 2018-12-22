

//val p = g.totalDegree

//println(p)

object a extends App {


  def LastElement[A](list: List[A]): A = {
    list.last
  }

  def LastElementEX[A](list: List[A]): A = {
    list match {
      case head :: Nil => head
      case _ :: tail => LastElementEX(tail)
      case _ => throw new NoSuchElementException
    }
  }

  def SecondLastElement[A](list: List[A]): A = {
    list match {
      case head :: _ :: Nil => head
      case _ :: tail => SecondLastElement(tail)
      case _ => throw new NoSuchElementException
    }
    //second way
    if (list.isEmpty) throw new NoSuchElementException
    else list.init.last

  }

  def pack[A](list: List[A]): List[List[A]] = {
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span {
        _ == list.head
      }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }


  def Generate_Combinations[A](list: List[A], number_of_objects: Int): List[List[A]] = {
    list.combinations(number_of_objects).toList
  }

  def encode[A](list: List[A]): List[(Int, A)] = {
    pack(list) map { x => (x.length, x.head) }
  }

  def List_Sort[A](list: List[List[A]]): List[List[A]] = {
    list.sortBy(_.length)

  }

  def Lists_Sort_By_Frequency[A](list: List[List[A]]): List[List[A]] = {
    val t = List_Sort(list)
    val tt = t.groupBy(_.length)
    val ttt = tt.values.toList
    val tttt = ttt.sortBy(_.length)
    tttt.flatten

  }

  def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def Group_Elements_Into_Disjoint_Subsets[A](group_sizes: List[Int], list: List[A]): List[List[List[A]]] = {
    group_sizes match {
      case Nil => List(Nil)
      case x :: group_sizes => Generate_Combinations(list, x) flatMap { y =>
        Group_Elements_Into_Disjoint_Subsets(group_sizes, list diff (y)) map {
          y :: _
        }
      }
    }
  }

  /// 0 indexed, index too big or neg doesnt work
  def Remove_Element_From_List[A](list: List[A], index: Int): (List[A], A) = {
    val R = list(list.length - index)
    val L = list.take(index) ::: list.takeRight(list.length - index - 1)
    (L, R)
  }

  def Insert_Element_At[A](list: List[A], index: Int, element: A): List[A] = {
    list.take(index) ::: element :: list.takeRight(list.length - index)
  }


  def funnybusiness(i: Int): Int = {
    if (i >= 10)
      i
    else
      funnybusiness(i + 1)
  }

  def Random_Select_Helper[A](list: List[A], n: Int, r: util.Random): List[A] = {
    if (n <= 0) Nil
    else {
      val pair = Remove_Element_From_List(list, r.nextInt(list.length))
      pair._2 :: Random_Select_Helper(pair._1, n - 1, r)
    }
  }

  def Random_Select_N[A](list: List[A], n: Int): List[A] = {
    val r = util.Random
    Random_Select_Helper(list, n, r)
  }

  def Permutations[A](list: List[A]): List[List[A]] = {
    list.permutations.toList
  }

  def flatten(ls: List[Any]): List[Any] =
  {
    ls flatMap {
      case ms: List[_] => flatten(ms)
      case e => List(e)
    }
}

  def rotateLeft[A](list: List[A], i: Int): List[A] = {
    val size = list.size
    list.drop(i % size) ++ list.take(i % size)
  }



  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }

  def compressFunctionalEX[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }

  //println(lsofls)

  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil       => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

  def rotateRight[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(size - (i % size)) ++ seq.take(size - (i % size))
  }

  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
  {
    (ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 }
    map { _._1 })
  }

  def Zip2[A](list : List[A]) : List[(A,A)] =
  {
    list.zip(list)
  }

  val lo = List(1,2,3)
  println(Zip2(lo))





  //val l3 = List.range(0,5)
  //println(l3)

  //1  2  3  4
  //dropRight size -  1
  //drop n

  val lsofls = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
  val ls2 = List("gas","gas", "gas", "powered", "toilet", "toilet", "touch", "poopoo")
  val ls = List("whyteepee","abc","coc","sus","joj","hohsis","did whatever it took", "piece of tape", "on the house")
val helpmejesus = (List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))

  ls.reverse
  //ls.rotate
  //println(LastElementEX(ls))
 // println(SecondLastElement(ls))
//println(pack(ls2))
//println(encode(ls2))
//  println(Lists_Sort_By_Frequency(lsofls))
 //   println(Group_Elements_Into_Disjoint_Subsets(helpmejesus._1, helpmejesus._2))
 // println(Remove_Element_From_List(ls2,2))

}
