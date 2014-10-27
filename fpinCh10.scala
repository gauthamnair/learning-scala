
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

	def dual[T](m: Monoid[T]): Monoid[T] = new Monoid[T] {
		def op(x: T, y: T): T = m.op(y, x)
		val zero = m.zero
	}

	object ex10_3 {
		def endoMonoid[T] = new Monoid[T => T] {
			def op(fa: T => T, fb: T => T): T => T = 
				x => fa(fb(x))
			val zero: T => T = x => x
		}

		def function1CompositionMonoid[T] = endoMonoid[T]
		def function1AndThenMonoid[T] = dual(endoMonoid[T])		

		def test {
			// how would I?
			// actually should use the foldMap and such
		}
	}

	object ex10_4 {
		def test {
			println("Missing exercise 10.4\n")
		}
	}

	object ex10_5 {
		def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
			as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
	}

	object ex10_6 {
		import ex10_5.foldMap
		import ex10_3.{function1CompositionMonoid, function1AndThenMonoid}
		def foldRight[T, R](xs: List[T], zero: R)(op: (T, R) => R): R = {
			val composedFunc = 
				foldMap(
					xs, function1CompositionMonoid[R])(
					(element: T) => (accumulatedSoFar: R) => op(element, accumulatedSoFar))
			composedFunc(zero)
		}

		def foldLeft[T, R](xs: List[T], zero: R)(op: (R, T) => R): R = {
			val composedFunc = 
				foldMap(
					xs, function1AndThenMonoid[R])(
					(element: T) => (accumulatedSoFar: R) => op(accumulatedSoFar, element))
			composedFunc(zero)
		}

		def test {
			val xs = List(1,2,3)
			def testRight {
				val summed = foldRight(xs, 0)(_ + _)
				assert(summed == 6)
				val reconstituted = foldRight(xs, Nil: List[Int])(_ :: _)
				assert(reconstituted == xs)
			}
			testRight
			def testLeft {
				val summed = foldLeft(xs, 0)(_ + _)
				assert(summed == 6)
				val reversed = foldLeft(xs, Nil: List[Int])((revd, elem) => elem :: revd)
				assert(reversed == xs.reverse)
			}
			testLeft		
		}
	}

	object ex10_7 {
		def foldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
			if (v.length == 0) {
				m.zero
			} else if (v.length == 1) {
				f(v(0))
			} else {
				val (left, right) = v.splitAt(v.length / 2)
				val leftResult = foldMap(left, m)(f)
				val rightResult = foldMap(right, m)(f)
				m.op(leftResult, rightResult)
			}
		}

		def test {
			// import ex10_1.intAddition
			val xs = Vector(1,2,3,4)
			val aslist = foldMap(xs, listMonoid[Int])(x => List(x, x))
			assert(aslist == List(1,1,2,2,3,3,4,4))
		}
	}

	object ex10_9 {
		case class Bounds(min: Int, max: Int)

		def combine(b1: Bounds, b2: Bounds) = 
			Bounds(b1.min min b2.min, b1.max max b2.max)

		case class OrderingInfo(bounds: Bounds, ordered: Boolean)

		def adjoin(left: OrderingInfo, right: OrderingInfo): OrderingInfo =  {
			val OrderingInfo(lb, lordered) = left
			val OrderingInfo(rb, rordered) = right

			val ordered = lordered && rordered && (lb.max <= rb.min)
			val bounds = combine(lb, rb)

			OrderingInfo(bounds, ordered)
		}

		val orderingMonoid = new Monoid[Option[OrderingInfo]] {
			def op(l: Option[OrderingInfo], r: Option[OrderingInfo]) = {
				val combined = 
					l.flatMap(loinfo => 
						r.map(roinfo => adjoin(loinfo, roinfo)))
				combined orElse l orElse r
			}
			def zero = None: Option[OrderingInfo]
		}

		import ex10_7.foldMap
		def isOrdered(v: IndexedSeq[Int]): Boolean = {
			val oInfo = foldMap(v, orderingMonoid)(
				x => Some(OrderingInfo(Bounds(x,x), true)))
			oInfo match {
				case Some(OrderingInfo(_, ordered)) => ordered
				case None => true
			}
		}

		def test {
			assert(isOrdered(Vector(1,2,3,10)))
			assert(!isOrdered(Vector(1,2,10,3)))
			assert(isOrdered(Vector[Int]()))
		}
	}

	def main(args: Array[String]) {
		ex10_1.test
		ex10_2.test
		ex10_3.test
		ex10_4.test
		ex10_6.test
		ex10_7.test
		ex10_9.test
	}

}