
// Monoids!

object ch10 {
	trait Monoid[T] {
		def op(x1: T, x2: T): T
		def zero: T
	}

	val stringMonoid = new Monoid[String] {
		def op(x1: String, x2: String): String = 
			x1 + x2
		val zero = ""
	}

	def listMonoid[T] = new Monoid[List[T]] {
		def op(x1: List[T], x2: List[T]) = x1 ++ x2
		val zero: List[T] = Nil
	}

	def testMonoid[T] (m: Monoid[T], opTests: ((T,T), T)*) {
		def testOneExample(a: T, b: T, r: T) {
			assert(m.op(a,b) == r)
			assert(m.op(a, m.zero) == a)
			assert(m.op(m.zero, a) == a)
			assert(m.op(b, m.zero) == b)
			assert(m.op(m.zero, b) == b)
		}
		opTests.foreach { case ((a,b), r) => testOneExample(a,b,r) }
		val allTestInputs = opTests.flatMap {case((a,b), r) => List(a,b,r)}
		val allTriples = allTestInputs.distinct.combinations(3).toList
		def testAssociate(a: T, b: T, c: T) {
			assert(m.op(m.op(a,b), c) == m.op(a, m.op(b,c)))
		}
		def testTriple(triple: Seq[T]) {
			triple.toList.permutations.toList.foreach {
				case x1 :: x2 :: x3 :: Nil => testAssociate(x1, x2, x3)
				case _ => throw new Exception
			}
		}
		allTriples.foreach { testTriple(_) }
	}

	object ex10_1 {
		val intAddition = new Monoid[Int] {
			def op(x1: Int, x2: Int) = x1 + x2
			val zero: Int = 0
		}
		val intMultiplication = new Monoid[Int] {
			def op(x1: Int, x2: Int) = x1 * x2
			val zero: Int = 1
		}
		val booleanOr = new Monoid[Boolean] {
			def op(x1: Boolean, x2: Boolean) = x1 || x2
			val zero: Boolean = false
		}
		val booleanAnd = new Monoid[Boolean] {
			def op(x1: Boolean, x2: Boolean) = x1 && x2
			val zero: Boolean = true
		}
		def test {
			testMonoid(intAddition, (1,1) -> 2, (1,2) -> 3)
			testMonoid(intMultiplication, (2,3) -> 6, (10,5) -> 50)
			testMonoid(booleanOr, 
				(true,true) -> true, (true,false) -> true,
				(false, false) -> false)
			testMonoid(booleanAnd,
				(true, true) -> true, (true, false) -> false,
				(false, false) -> false)

		}
	}

	object ex10_2 {
		def optionMonoid[T] = new Monoid[Option[T]] {
			def op(x: Option[T], y: Option[T]): Option[T] = 
				x.orElse(y)
			val zero: Option[T] = None
		}
		def test {
			val intOptionMonoid: Monoid[Option[Int]] = optionMonoid
			testMonoid(intOptionMonoid,
				(Some(1), Some(2)) -> Some(1),
				(Some(2), Some(1)) -> Some(2),
				(None, Some(3)) -> Some(3))
		}
	}

	def main(args: Array[String]) {
		ex10_1.test
		ex10_2.test
	}

}