// 
// scalac fpinCh05.scala ; scala ch05

sealed trait Stream[+T] {
	def toList: List[T]
	def take(n: Int): Stream[T]
	def takeWhile(satisfies: T => Boolean): Stream[T]
	def headOption: Option[T]
	final def exists(satisfies: T => Boolean): Boolean = 
		this match {
			case Cons(hF, tF) => satisfies(hF()) || tF().exists(satisfies)
			case _ => false
		}
	final def foldRight[R](zero: => R)(op: (T, => R) => R): R = 
		this match {
			case Empty => zero
			case Cons(hF, tF) => op(hF(), tF().foldRight(zero)(op))
		}
	@annotation.tailrec
	final def forall(satisfies: T => Boolean): Boolean =
		this match {
			case Empty => true
			case Cons(hF, tF) => {
				if (satisfies(hF())) tF().forall(satisfies)
				else false
			}
		}
	final def map[R](f: T => R): Stream[R] = 
		this.foldRight(Empty: Stream[R])(
			(a, b) => Stream.cons(f(a), b))
	final def filter(satisfies: T => Boolean): Stream[T] =
		this.foldRight(Empty: Stream[T])(
			(a, b) => {
				if (satisfies(a)) Stream.cons(a, b) 
				else b
				})
	final def append[T2 >: T](next: => Stream[T2]): Stream[T2] =
		this.foldRight(next)(Stream.cons(_, _))
	final def flatMap[R](f: T => Stream[R]): Stream[R] =
		this.foldRight(Empty: Stream[R])((a, b) => f(a).append(b))
	final def find(satisfies: T => Boolean): Option[T] =
		this.filter(satisfies).headOption
}


case object Empty extends Stream[Nothing] {
	def toList: List[Nothing] = Nil
	def take(n:Int): Stream[Nothing] = this
	def takeWhile(satisfies: Nothing => Boolean) = this
	def headOption: Option[Nothing] = None
}
// This declaration below does *not* work: 
// case class Cons[+T](headExpr: => T, tailExpr: => Stream[T]) extends Stream[T]
// val parameters to a case class cannot be call-by-name. Therefore we send in 
// function values (a loophole!)
case class Cons[+T](headF: () => T, tailF: () => Stream[T]) extends Stream[T] {
	def toList: List[T] = headF() :: (tailF().toList)
	def take(n:Int): Stream[T] = {
		if (n <= 0) Empty
		else Cons(headF, () => tailF().take(n-1))
	}
	def takeWhile(satisfies: T => Boolean): Stream[T] = {
		if (satisfies(headF())) Cons(headF, () => tailF().takeWhile(satisfies))
		else Empty
	}
	def headOption: Option[T] = Some(headF())
}

object Stream {
	def cons[T](headExpr: => T, tailExpr: => Stream[T]): Stream[T] = {
		// converts the expressions, which could
		// would be called every time 
		lazy val headValue = headExpr
		lazy val tailValue = tailExpr
		Cons(() => headValue, () => tailValue)
	}
	def empty[T]: Stream[T] = Empty
	def emptyLike[T](template: T): Stream[T] = Empty
	def apply[T](xs: T*): Stream[T] =
	if (xs.isEmpty) empty else cons(xs.head, apply(xs.tail: _*))

	def headOption[T](xs: Stream[T]): Option[T] = 
	xs match {
		case Cons(hF, tF) => Some(hF())
		case Empty => None
	}

	def tailOption[T](xs: Stream[T]): Option[Stream[T]] = 
	xs match {
		case Cons(hF, tF) => Some(tF())
		case Empty => None
	}

	def constant[T](x: T): Stream[T] = Stream.cons(x, constant(x))

	val ones = constant(0)

	def from(n: Int = 0): Stream[Int] = Stream.cons(n, from(n + 1))

	def test {
		val s = Stream.cons(1, Stream.empty)
		assert(headOption(s) == Some(1))
		val xs = Stream.apply(3,2,1)
		assert(headOption(xs) == Some(3))
	}
}

object ex51 {
	def test {
		val s = Stream.cons("cat", Stream.empty)
		assert(s.toList == List("cat"))
		val xs = Stream.apply(3,2,1)
		assert(xs.toList == List(3,2,1))
		assert(Stream.empty.toList == Nil)
	}
}

object ex52 {
	def test {
		val s = Stream.cons("cat", Stream.empty)
		assert(s.take(1).toList == List("cat"))
		val xs = Stream.apply(3,2,1)
		assert(xs.take(2).toList == List(3,2))
		assert(xs.take(0).toList == Nil)
		assert(xs.take(10).toList == List(3,2,1))
		assert(Stream.empty.take(0).toList == Nil)
	}
}

object ex53 {
	def test {
		val s = Stream.cons("cat", Stream.empty)
		assert(s.takeWhile(_ == "cat").toList == List("cat"))
		assert(s.takeWhile(_ == "dog").toList == Nil)
		val xs = Stream.apply(3,2,1)
		assert(xs.takeWhile(_ > 1).toList == List(3,2))
		assert(xs.takeWhile(_ > 0).toList == List(3,2,1))
		assert(xs.takeWhile(_ > 5).toList == Nil)
	}
}


object ch05 {
	object lazyPlay {
		def if2[T](cond: Boolean,
			onTrue: => T, onFalse: => T): T = {
			if (cond) onTrue else onFalse
		}
		var counter = 0
		def called(x: Int): Int = {
			counter = counter + 1
			x
		}
		def test {
			assert(counter == 0)
			called(1)
			assert(counter == 1)
			if2(true, 1, called(2))
			assert(counter == 1)
			if2(true, called(1), called(2))
			assert(counter == 2)
		}
	}

	object testFoldRight {
		def contains[T](xs: Stream[T])(satisfies: T => Boolean): Boolean =
			xs.foldRight(false)((x,y) => satisfies(x) || y)
		def test {
			val xs = Stream(1,2,3)
			assert(xs.foldRight(0)(_ + _) == 6)
			assert(contains(xs)(_ > 2))
			assert(!contains(xs)(_ > 4))
		}
	}

	object ex54 {
		def test {
			val xs = Stream(2,4,6)
			assert(xs.forall(_ % 2 == 0))
			assert(! Stream(2,5,6).forall(_ % 2 == 0))
		}
	}

	object ex55 {
		def takeWhile[T](xs: Stream[T])(satisfies: T=> Boolean): Stream[T] =
			xs.foldRight(Stream.empty: Stream[T])(
				(a, b) => {
					if (!satisfies(a)) Stream.empty 
					else Stream.cons(a, takeWhile(b)(satisfies))
				})
		def test {
			val xs = Stream(1,2,3)
			assert(takeWhile(xs)(_ < 3).toList == List(1,2))
			assert(takeWhile(xs)(_ < 2).toList == List(1))
			assert(takeWhile(xs)(_ > 5).toList == Nil)
		}
	}

	object ex56 {
		def headOption[T](xs: Stream[T]): Option[T] = 
			xs.foldRight(None: Option[T])((a, b) => Some(a))
		def test {
			val xs = Stream(1,2,3)
			assert(headOption(xs) == Some(1))
			assert(headOption(Stream.empty) == None)
		}
	}

	object ex57 {
		def toDbledStream[T](x: T): Stream[T] = Stream(x, x)

		def test {
			assert(Stream(1,2,3).map(2 * _).toList == List(2,4,6))
			assert(Stream.emptyLike(1).map(2 * _).toList == Nil)
			assert(Stream("a","b").map(_.toUpperCase).toList == List("A","B"))

			assert(Stream(1,2,3).filter(_ > 2).toList == List(3))
			assert(Stream(1,2,3).filter(_ % 2 == 1).toList == List(1,3))
			assert(Stream(1,2,3).filter(_ > 5).toList == Nil)

			assert(Stream(1,2).append(Stream(3)).toList == List(1,2,3))
			assert(Stream().append(Stream(3)).toList == List(3))
			assert(Stream(1,2).append(Stream()).toList == List(1,2))

			assert(Stream(1,2).flatMap(toDbledStream(_)).toList == List(1,1,2,2))

			assert(Stream(1,2,3).find(_ > 1).getOrElse(-1) == 2)
			assert(Stream(1,2,3).find(_ > 4).getOrElse(-1) == -1)
		}
	}

	object ex58 {
		def test {
			assert(Stream.constant(7).take(3).toList == List(7,7,7))
			assert(Stream.constant(5).map(2*_).take(3).toList == List(10,10,10))
		}
	}

	object ex59 {
		def test {
			assert(Stream.from(0).take(3).toList == List(0,1,2))
			assert(Stream.from(0).find(_ > 20).getOrElse(-1) == 21)
		}
	}

	object ex510 {
		def fibs: Stream[Int] = {
			def fibStep(lastTwo: (Int, Int)): Stream[(Int, Int)] = {
				val nextTwo = (lastTwo._2, lastTwo._1 + lastTwo._2)
				Stream.cons(nextTwo, fibStep(nextTwo))
			}
			val fibSteps: Stream[(Int, Int)] = Stream.cons((0,1), fibStep((0, 1)))
			// In this arrangement we compute the n+1th fib number before returning the nth.
			fibSteps.map(_._1)
		}
		def test {
			assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
		}
	}

	object ex511 {
		def unfold[R, S](startingState: S)(step: S => Option[(R, S)]): Stream[R] = {
			step(startingState) match {
				case Some((newResult, nextState)) => Stream.cons(newResult, unfold(nextState)(step))
				case None => Stream.empty
			}
		}
		case class FibState(lagging: Int, leading: Int)
		def fibStep(state: FibState): (Int, FibState) =
			(state.lagging, FibState(state.leading, state.leading + state.lagging))
		def fibs: Stream[Int] = unfold(FibState(0,1))(state => Some(fibStep(state)))

		def from(n: Int): Stream[Int] = 
			unfold(n)(x => Some(Tuple2(x, x + 1)))
		def constant[T](theValue: T): Stream[T] = 
			unfold(None)(x => Some(Tuple2(theValue, None)))
		def test {
			assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
		}
	}

	object ex512 {
		import ex511._
		def test {
			assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
			assert(from(0).take(3).toList == List(0,1,2))
			assert(from(0).find(_ > 20).getOrElse(-1) == 21)
			assert(constant(7).take(3).toList == List(7,7,7))
			assert(constant(5).map(2*_).take(3).toList == List(10,10,10))
			// ommitted ones test. it is just constant(1)
		}
	}

	object ex513 {
		import ex511.unfold
		def map[T,R](xs: Stream[T])(f: T => R): Stream[R] = 
			unfold(xs)(x => x match {
				case Cons(hF, tF) => Some(Tuple2(f(hF()), tF()))
				case Empty => None
				})

		def zip[A, B](as: Stream[A], bs: Stream[B]): Stream[(A, B)] =
			unfold(Tuple2(as, bs))({
					case Tuple2(Cons(ahF, atF), Cons(bhF, btF)) => {
						val heads = (ahF(), bhF())
						val tails = (atF(), btF())
						Some(heads, tails)
					}
					case _ => None
					})

		def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
			unfold(Tuple2(as,bs))({
					case Tuple2(Cons(ahF, atF), Cons(bhF, btF)) => {
						val heads = (Some(ahF()), Some(bhF()))
						val tails = (atF(), btF())
						Some(heads, tails)
					}
					case Tuple2(Cons(ahF, atF), Empty) => {
						val heads = (Some(ahF()), None)
						val tails = (atF(), Empty)
						Some(heads, tails)
					}
					case Tuple2(Empty, Cons(bhF, btF)) => {
						val heads = (None, Some(bhF()))
						val tails = (Empty, btF())
						Some(heads, tails)
					}
					case _ => None
				})

		def zipWith[A, B, R](as: Stream[A], bs: Stream[B])
						(op: (A, B) => R): Stream[R] = zip(as, bs).map({case (a,b) => op(a,b)})

		def take[T](xs: Stream[T], n:Int): Stream[T] = 
			unfold(zip(Stream.from(0), xs))({
				case Cons(headsF, tailsF) => {
					val (i, x) = headsF()
					if (i < n) Some((x, tailsF())) else None
				}
				case Empty => None
				} )

		def takeWhile[T](xs: Stream[T])(satisfies: T => Boolean): Stream[T] = 
			unfold(xs)({
				case Cons(headsF, tailsF) => {
					val theHead = headsF()
					if (satisfies(theHead)) Some((theHead, tailsF())) else None
				}
				case Empty => None
				})


		def test {
			assert(map(Stream(1,2,3))(x => x).toList == List(1,2,3))
			assert(map(Stream(1,2,3))(x => 2*x).toList == List(2,4,6))
			assert(map(Stream.from(0))(x => 2*x).take(3).toList == List(0,2,4))

			val zipped = zip(Stream.from(0), Stream("a","b","c"))
			assert(zipped.toList == List((0, "a"), (1, "b"), (2, "c")))

			val zippedWith = zipWith(Stream.from(0), Stream.constant(2))(_ + _)
			assert(zippedWith.take(4).toList == List(2,3,4,5))

			assert(take(Stream.from(2), 3).toList == List(2,3,4))
			assert(takeWhile(Stream.from(0))(_ < 3).toList == List(0,1,2))

			val allZipped = zipAll(Stream(1,2,3,4), Stream(1,2))
			assert(allZipped.toList == List((Some(1), Some(1)), 
				(Some(2),Some(2)), (Some(3), None), (Some(4), None) ))
		}
	}

	def main(args: Array[String]) {
		Stream.test
		lazyPlay.test		
		ex51.test
		ex52.test
		ex53.test
		testFoldRight.test
		ex54.test
		ex55.test
		ex56.test
		ex57.test
		ex58.test
		ex59.test
		ex510.test
		ex511.test
		ex512.test
		ex513.test
	}
}