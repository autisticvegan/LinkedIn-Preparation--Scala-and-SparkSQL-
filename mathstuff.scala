import scala.math._
import scala.collection.mutable.HashSet
  object i extends App {

    var h = BigInt(32416190071L)
    h.gcd(32416190072L)
    println(h.isProbablePrime(9000))

    def Is_Prime_Range(in: Int): Boolean = {
      !(2 to scala.math.sqrt(in).toInt).exists(x => in % x == 0)
    }

    def Is_Prime(in: Int): Boolean = {
      (in > 1) && Is_Prime_Range(in)
    }

    def isPrime(num: Int): Boolean =
      (num > 1) && !(2 to scala.math.sqrt(num).toInt).exists(x => num % x == 0)

    def List_Primes_In_Range(low: Int, high: Int): List[Int] = {
      val ret = List.range(low, high)

      ret.filter(Is_Prime(_))
    }

    //    println(List_Primes_In_Range(0,100))


    def Find_Pair(int: Int, target: Int, hash: HashSet[Int]): Option[(Int,Int)] = {
      if(hash.contains(target - int))
        Option(int,target-int)
      else
        None
    }

    //if find pair, then put into a pair, and append to list

    //for each number, minus it from the target, and check if result is there
    def GoldBach(target : Int, hash : HashSet[Int]) : Option[(Int,Int)] =
      {
        //list of prime numbers

        //iterate over them, if they have

        //if true, we have target - int, and int

        var ret : Option[(Int,Int)] = None
        //3rd thing is target, target-int, int
        hash.takeWhile { x => Find_Pair(x,target,hash) match {
          case Some(y) => ( ret = Some(y) ); false
          case None => true
        }}

        ret
      }

    //need thing that returns a triple
    def Get_Triple(target : Int, hash : HashSet[Int]) : Option[(Int,Int)] =
    {
      GoldBach(target, hash)
    }

    def GoldBach_List(range : Range): List[(Int,Int,Int)] =
    {
      //list of numbers

      //find 2 primes that add to them

      //list of primes as a hash set

      val hashSet = HashSet() ++ List_Primes_In_Range(2,5000)

      (for ( x <- range; res <- Get_Triple(x,hashSet)) yield (x,res._1,res._2)).toList
    }


    println(GoldBach_List(1950 to 2000))
  }


