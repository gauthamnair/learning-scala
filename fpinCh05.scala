// 
// scalac fpinCh05.scala ; scala ch05

sealed trait Stream[+T] {
	def toList: List[T]
	def take(n: Int): Stream[T]
	def takeWhile(satisfies: T => Boolean): Stream[T]
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
}


case object Empty extends Stream[Nothing] {
	def toList: List[Nothing] = Nil
	def take(n:Int): Stream[Nothing] = this
	def takeWhile(satisfies: Nothing => Boolean) = this
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
	}
}