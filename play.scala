
object myList {
	trait List[+T] {
		def map[R](f: T => R): List[R]
		def foldLeft[R](zero: R)(f: (R,T) => R): R
		def foldRight[R](zero: R)(f: (T, R) => R): R
		def flatMap[R]
		def filter(f: T => Boolean): List[T]
	}

	case class NonEmpty[+T](head: T, tail: List[T]) extends List[T] {
		def map[R](f: T => R): List[R] = 
			NonEmpty(f(head), tail.map(f))

		// cannot tailrec because tail is a List[T]
		def foldLeft[R](zero: R)(op: (R,T) => R): R = 
			tail.foldLeft(op(zero, head))(op)

		def foldRight[R](zero: R)(op: (T, R) => R): R = 
			op(head, tail.foldRight(zero)(op))

		def filter(f: T => Boolean): List[T] = {
			if (f(head)) NonEmpty(head, tail.filter(f))
			else tail.filter(f)
		}

		override def toString = "(" + head.toString + "::" + tail.toString + ")"
	}

	case object Nil extends List[Nothing] {
		def map[R](f: Nothing => R): List[R] = this

		def foldLeft[R](zero: R)(op: (R,Nothing) => R): R = zero

		def foldRight[R](zero: R)(op: (Nothing, R) => R): R = zero

		def filter(f: Nothing => Boolean): List[Nothing] = Nil

		override def toString = "Nil"
	}

	def test {
		val xs: List[Int] = NonEmpty(3, Nil)
		val ys: List[Int] = NonEmpty(2, xs)
		println(ys)

		val ysquared = ys.map(x => x*x)
		println(ysquared)
		println(ysquared.foldLeft(0)(_ + _))
	}
}