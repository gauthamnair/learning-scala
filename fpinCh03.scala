
sealed trait List[+T]

case class Cons[T](head: T, tail: List[T]) extends List[T]
case object Nil extends List[Nothing]

object List {
	def sumRecurse(as: List[Int]): Int = as match {
		case Cons(x, xs) => x + sum(xs)
		case Nil => 0
	}

	def sum(as: List[Int]): Int = foldRight(as, 0)(_ + _)

	def lazyMultiply(a: Double, b: => Double): Double = {
		if (a == 0.0) 0.0 else {
			println(a.toString + "*" + b.toString)
			a * b
		}
	}

	def product(as: List[Double]): Double = foldRight(as, 1.0)(_ * _)

	def productStopIf0Recurse(as: List[Double]): Double = 
		as match {
			case Nil => 1.0
			case Cons(x, rest) => {
				if (x != 0.0) x * productStopIf0(rest) else 0
			}
		}

	def productStopIf0(as: List[Double]): Double = 
		foldRight(as, 1.0)(lazyMultiply(_, _))

	def apply[T](as: T*): List[T] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def tail[T](as: List[T]): Option[List[T]] = as match {
		case Cons(h, t) => Some(t)
		case Nil => None
	}

	def drop[T](as: List[T], n: Int): Option[List[T]] =
		if (n > 0) {
			as match {
				case Cons(h, t) => drop(t, n - 1)
				case Nil => None
			}
		} else Some(as)

	def foldRight[T, R](as: List[T], zero: R)
						(op: (T, R) => R): R =
		as match {
			case Nil => zero
			case Cons(x, rest) => {
				println("requesting op with first arg:" + x.toString)
				lazy val remaining = foldRight(rest, zero)(op)
				op(x, remaining)
			}
		}

	def init[T](as: List[T]): Option[List[T]] =
		as match {
			case Cons(y, Nil) => Some(Nil)
			case Cons(x, y) => init(y).map(Cons(x, _))
			case Nil => None
		}

	@annotation.tailrec
	def dropWhile[T](as: List[T])
		(shouldDrop: T => Boolean): List[T] = 
		as match {
			case Cons(h, t) => {
				if (shouldDrop(h)) dropWhile(t)(shouldDrop)
				else Cons(h, t)
			}
			case Nil => Nil
		}

	def setHead[T](as: List[T], a: T): Option[List[T]] = 
		as match {
			case Cons(h, t) => Some(Cons(a, t))
			case Nil => None
		}
}

object testList {
	def testtail {
		assert(List.tail(List(1,2)) == Some(List(2)))
		assert(List.tail(List(1)) == Some(Nil))
		assert(List.tail(Nil) == None)
	}
	def testInit {
		val xs = List(1,2,3,4)
		assert(List.init(xs) == Some(List(1,2,3)))
		assert(List.init(List(1)) == Some(Nil))
		assert(List.init(List()) == None)
	}
	def testSetHead {
		assert(List.setHead(List(1,2), 3) == Some(List(3,2)))
		assert(List.setHead(List("cat"), "dog") == Some(List("dog")))
		assert(List.setHead(Nil, 3) == None)
	}
	def testDropWhile {
		val xs = List(2,4,6,1,3)
		val withOddHead = List.dropWhile(xs)(_ % 2 == 0)
		assert(withOddHead == List(1,3))
		val alldropped = List.dropWhile(xs)(x => true)
		assert(alldropped == Nil)
		val nilOddHead = List.dropWhile(Nil: List[Int])(_ % 2 == 0)
		assert(nilOddHead == Nil)
	}
	def testDrop {
		val xs = List(1,2,3,4,5)
		assert(List.drop(xs, 3) == Some(List(4,5)))
		assert(List.drop(xs, 5) == Some(Nil))
		assert(List.drop(xs, 6) == None)
	}
	def test {
		assert(List(1, 2) == Cons(1, Cons(2, Nil)))
		assert(List("a", "b") == Cons("a", Cons("b", Nil)))
		assert(List.sum(List(1,2)) == 3)
		testtail
		testSetHead
		testDrop
		testDropWhile
		testInit
	}
}
testList.test


object ex31 {
	def test {
		val x = List(1,2,3,4,5)
		val m = x match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + List.sum(t)
			case _ => 101
		}
		assert(m == 1 + 2)
	}
}
ex31.test