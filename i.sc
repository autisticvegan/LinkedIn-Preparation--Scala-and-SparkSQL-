


object polys {

	class Poly(val terms0: Map[Int, Double]) {
	//the asterisk means arbitrary number of params
	  def this(bindings: (Int, Double)*) = this(bindings.toMap)
		val terms = terms0 withDefaultValue 0.0
	//++ is not concat, but superimpose here
	//we need something that adds 2 coefficients -> break it out into other functions
	//	def + (other: Poly) = terms ++ other.terms)
		//def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
		
		//can also implement + using foldLeft...which is more efficient? (fold left)
		def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
		def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
			val (exp, coeff) = term
			terms + (exp -> (coeff + terms(exp)))
		}
		
		
		def adjust(term: (Int, Double)): (Int, Double) = {
			val (exp, coeff) = term
			exp -> (coeff + terms(exp))
//if not using default value need below instead of 2nd line
//			terms get exp match {
//				case Some(coeff1) => exp -> (coeff + coeff1) //arrow and sum have same prec
//				case None => exp -> coeff
//			}
		
	}
		override def toString =
			(for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
	
	}

//is there a way we can make Poly without passing the intermediate data struct? we need a variadic function


	val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
	
	val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
	p1 + p2
}