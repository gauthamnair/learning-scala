
sealed trait List[+T]

case class Cons[T](head: T, tail: List[T]) extends List[T]
case object Nil extends List[Nothing]

object List {
	def sumRecurse(as: List[Int]): Int = as match {
		case Cons(x, xs) => x + sum(xs)
		case Nil => 0
	}

	def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

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

	@annotation.tailrec
	def foldLeft[T, R](as: List[T], accumulated: R)
					(op: (R, T) => R): R =
		as match {
			case Nil => accumulated
			case Cons(x, rest) => foldLeft(rest, op(accumulated, x))(op)
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

object ex38 {
	def test {
		val x = List(1,2,3)
		val byFold = List.foldRight(x, Nil: List[Int])(Cons(_, _))
		assert(byFold == x)
	}
}
ex38.test

object ex39 {
	def length[T](as: List[T]): Int = List.foldRight(as, 0)((x, y) => 1 + y)
	def test {
		val x = List(1,2,3)
		assert(length(x) == 3)
		assert(length(Nil) == 0)
	}
}
ex39.test

object ex311 {
	def length[T](as: List[T]): Int = List.foldLeft(as, 0)((x, y) => x + 1)
	def test {
		val x = List(1,2,3)
		assert(length(x) == 3)
		assert(length(Nil) == 0)
	}
}
ex311.test

object ex312 {
	def reversed[T](as: List[T]): List[T] = List.foldLeft(as, Nil: List[T])((x,y) => Cons(y, x))
	def test {
		val x = List(1,2,3)
		assert(reversed(x) == List(3,2,1))
		assert(reversed(Nil) == Nil)
	}
}
ex312.test

object ex313 {
	def foldRight[T, R](as: List[T], accumulated: R)(op: (T, R) => R): R = {
		val reversedAs = ex312.reversed(as)
		List.foldLeft(reversedAs, accumulated)((x,y) => op(y,x))
	}
	def sum(as: List[Int]): Int = foldRight(as, 0)(_ + _)
	def reconstruct[T](as: List[T]): List[T] = foldRight(as, Nil: List[T])(Cons(_, _))
	def test {
		val x = List(1,2,3)
		assert(sum(x) == 6)
		assert(reconstruct(x) == x)
	}
}
ex313.test

object ex314 {
	def append[T](as: List[T], bs: List[T]): List[T] = ex313.foldRight(as, bs)(Cons(_, _))
	def test {
		val x1 = List(1,2,3)
		val x2 = List(4,5)
		assert(append(x1, x2) == List(1,2,3,4,5))
	}
}
ex314.test

object ex315 {
	def flatten[T](lists: List[List[T]]): List[T] = 
		ex313.foldRight(lists, Nil: List[T])(ex314.append(_, _))
	def test {
		val x1 = List(1,2)
		val x2 = List(3)
		val x3 = List(4, 5)
		val xs = List(x1, x2, x3)
		assert(flatten(xs) == List(1,2,3,4,5))
	}
}
ex315.test


object ex316 {
	def addOne(xs: List[Int]): List[Int] = xs match {
		case Cons(x, rest) => Cons(x + 1, addOne(rest))
		case Nil => Nil
	}
	def test {
		val xs = List(1, 3, 4)
		assert(addOne(xs) == List(2, 4, 5))
		assert(addOne(Nil) == Nil)
	}
}
ex316.test

object ex317 {
	def dbls2Strings(xs: List[Double]): List[String] = xs match {
		case Cons(x, rest) => Cons(x.toString, dbls2Strings(rest))
		case Nil => Nil
	}
	def test {
		val xs = List(1.4, 3.4, 4.4)
		assert(dbls2Strings(xs) == List("1.4", "3.4", "4.4"))
	}
}
ex317.test

object ex318 {
	def map[T, R](xs: List[T])(f: T => R): List[R] = xs match {
		case Cons(x, rest) => Cons(f(x), map(rest)(f))
		case Nil => Nil
	}
	def addOne(xs: List[Int]): List[Int] = map(xs)(_ + 1)
	def dbls2Strings(xs: List[Double]): List[String] = map(xs)(_.toString)
	def test {
		val xs = List(1, 3, 4)
		assert(addOne(xs) == List(2, 4, 5))
		assert(addOne(Nil) == Nil)
		val ds = List(1.4, 3.4, 4.4)
		assert(dbls2Strings(ds) == List("1.4", "3.4", "4.4"))
	}
}
ex318.test

object ex319 {
	def filter[T](xs: List[T])(satisfies: T => Boolean): List[T] = 
		xs match {
			case Cons(x, rest) => {
				if (satisfies(x)) Cons(x, filter(rest)(satisfies))
				else filter(rest)(satisfies)
			}
			case Nil => Nil
		}
	def removeOdds(xs: List[Int]): List[Int] = filter(xs)(_ % 2 == 0)
	def test {
		val xs = List(1,2,3,4,5)
		assert(removeOdds(xs) == List(2, 4))
	}
}
ex319.test

object ex320 {
	def flatMap[T, R](xs: List[T])(f: T => List[R]): List[R] =
		xs match {
			case Cons(x, rest) => ex314.append(f(x), flatMap(rest)(f))
			case Nil => Nil
		}
	def test {
		val xs = List(1,2,3)
		val doubled = flatMap(xs)(x => List(x, x))
		assert(doubled == List(1,1,2,2,3,3))
	}
}
ex320.test

object ex321 {
	import ex320.flatMap
	def filter[T](xs: List[T])(satisfies: T => Boolean): List[T] =
		flatMap(xs)(x => if (satisfies(x)) List(x) else Nil)
	def removeOdds(xs: List[Int]): List[Int] = filter(xs)(_ % 2 == 0)
	def test {
		val xs = List(1,2,3,4, 5)
		assert(removeOdds(xs) == List(2, 4))
	}
}
ex321.test

object ex322 {
	def padd(as: List[Int], bs: List[Int]): List[Int] =
		as match {
			case Cons(a, arest) => {
				bs match {
					case Cons(b, brest) => Cons(a + b, padd(arest, brest))
					case Nil => Nil
				}
			}
			case Nil => Nil
		}
	def test {
		val as = List(1,2,3)
		val bs = List(4,5,6)
		assert(padd(as, bs) == List(5, 7, 9))
	}
}
ex322.test

object ex323 {
	def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] =
		as match {
			case Cons(a, arest) => {
				bs match {
					case Cons(b, brest) => Cons((a, b), zip(arest, brest))
					case Nil => Nil
				}
			}
			case Nil => Nil
		}
	def zipWith[A, B, R](as: List[A], bs: List[B])
						(op: (A, B) => R): List[R] =
						ex318.map(zip(as, bs))( (ab:(A,B)) => op(ab._1, ab._2))
	def test {
		val as = List(1,2,3)
		val bs = List(4,5,6)
		assert(zip(as, bs) == List((1,4), (2,5), (3, 6)))
		assert(zipWith(as, bs)(_ + _) == List(5, 7, 9))
	}
}
ex323.test

object ex324 {
	import ex323.zipWith
	import ex311.length
	def listHeadsMatch[T](xs: List[T], sub: List[T]): Boolean = {
		val itemsMatch = zipWith(xs, sub)(_ == _)
		val allMatch = List.foldLeft(itemsMatch, true)(_ & _)
		val lengthIsCorrect = length(itemsMatch) == length(sub)
		allMatch & lengthIsCorrect
	}

	@annotation.tailrec
	def hasSubsequence[T](xs: List[T], sub: List[T]): Boolean = {
		xs match {
			case Nil => false
			case Cons(x, xrest) => {
				if (listHeadsMatch(xs, sub)) true
				else hasSubsequence(xrest, sub)
			}
		}
	}

	def test {
		val xs = List(3,2,4)
		val ys = List(3,2)
		assert(listHeadsMatch(xs, ys))
		assert(listHeadsMatch(xs, List(): List[Int]))
		assert(!listHeadsMatch(xs, List(1,2)))

		assert(hasSubsequence(xs, List(3,2)))
		assert(hasSubsequence(xs, List(2,4)))
		assert(!hasSubsequence(xs, List(4,2)))
	}
}
ex324.test