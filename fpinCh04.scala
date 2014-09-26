

sealed trait Option[+T] {
	def map[R](f: T => R): Option[R]
	def flatMap[R](f: T => Option[R]): Option[R]
	// this is the "extractor" from the option
	// it allows you to not return something of
	// type T but rather of a supertype of T.
	def getOrElse[R >: T](default: => R): R
	// example: get a file, but it fails so None.
	// then can chain orElse on the Option with
	// a function that looks for the file in another
	// location, which could itself return None
	def orElse[R >: T](alternative: => Option[R]): Option[R]
	// converts Some to none if the value does not satisfy
	def filter(satisfies: T => Boolean): Option[T]
}

case object None extends Option[Nothing] {
	def map[R](f: Nothing => R): Option[R] = 
		None
	def flatMap[R](f: Nothing => Option[R]): Option[R] = 
		None
	def getOrElse[R >: Nothing](default: => R): R = 
		default
	def orElse[R >: Nothing](alternative: => Option[R]): Option[R] = 
		alternative
	def filter(satisfies: Nothing => Boolean): Option[Nothing] = 
		None
}

case class Some[T](value: T) extends Option[T] {
	def map[R](f: T => R): Option[R] = 
		Some(f(value))
	def flatMap[R](f: T => Option[R]): Option[R] = 
		f(value)
	def getOrElse[R >: T](default: => R): R = 
		value
	def orElse[R >: T](alternative: => Option[R]): Option[R] = 
		this
	def filter(satisfies: T => Boolean): Option[T] = 
		if (satisfies(value)) this else None
}

def mean(xs: Seq[Double]): Option[Double] =
	if (xs.isEmpty) None else Some(xs.sum / xs.length)

def testMean {
	assert(mean(List(1.0, 2.0)) == Some(1.5))
	assert(mean(List(): List[Double]) == None)
}
testMean

object ex41 {
	def test {
		val sx = Some(3)
		assert(sx.map(2 * _) == Some(6))
		assert(sx.flatMap(x => Some(2 * x)) == Some(6))
		assert(sx.flatMap(x => None) == None)
		assert(sx.getOrElse(0) == 3)
		assert(sx.orElse(None) == Some(3))
		assert(sx.orElse(Some(5)) == Some(3))
		assert(sx.filter(_ > 2) == Some(3))
		assert(sx.filter(_ < 2) == None)

		val nx: Option[Int] = None
		assert(nx.map(2 * _) == None)
		assert(nx.flatMap(x => Some(2 * x)) == None)
		assert(nx.flatMap(x => None) == None)
		assert(nx.getOrElse(0) == 0)
		assert(nx.orElse(None) == None)
		assert(nx.orElse(Some(5)) == Some(5))
		assert(nx.filter(_ > 2) == None)
		assert(nx.filter(_ < 2) == None)
	}
}
ex41.test

object ex42 {
	def mean(xs:Seq[Double]): Option[Double] = 
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

	def summedSquareDev(xs:Seq[Double], center:Double): Double =
		xs.map(_ - center).map((x) => x * x).sum

	def normalizedSampleSquareDev(xs:Seq[Double], 
								center:Double): Option[Double] =
		if (xs.length <= 1) None
		else Some(summedSquareDev(xs, center)/(xs.length - 1))

	def sampleVariance(xs: Seq[Double]): Option[Double] =
		mean(xs).flatMap(normalizedSampleSquareDev(xs, _))
		

	def test {
		val xs = List(1.0, 2.0)
		assert(mean(xs) == Some(1.5))
		assert(mean(List(1.0)) == Some(1.0))
		assert(mean(List(): Seq[Double]) == None)

		assert(sampleVariance(xs) == Some(0.5))
		assert(sampleVariance(List(1.0)) == None)
		assert(sampleVariance(List(): Seq[Double]) == None)
	}
}
ex42.test


object ex43{
	def map2[A, B, R](a: Option[A], b:Option[B])
					(f: (A, B) => R): Option[R] =
		a.flatMap(anA => b.map(aB => f(anA, aB)))

	def test {
		assert(map2(Some(1), Some(2))(_ + _) == Some(3))
		assert(map2(None: Option[Int], Some(2))(_ + _) == None)
		assert(map2(Some(1), None: Option[Int])(_ + _) == None)
	}
}
ex43.test


object ex44 {
	import ex43.map2
	def sequence[T](xos: List[Option[T]]): Option[List[T]] = 
		xos match {
			case xo :: resto => map2(xo, sequence(resto))(_ :: _)
			case Nil => Some(Nil)
		}

	def test {
		assert(sequence(Some(2) :: Some(1) :: Nil) == Some(2 :: 1 :: Nil))
		assert(sequence(Some(2) :: None :: Nil) == None)
		assert(sequence(Nil) == Some(Nil))
	}
}
ex44.test


def Try[T](t: => T): Option[T] =
	try {Some(t)} catch { case e: Exception => None } 

def testTry {
	assert(Try("a".toInt) == None)
	assert(Try("123".toInt) == Some(123))
}
testTry




object ex45 {
	def parseIntsWithSeq(as: List[String]): Option[List[Int]] =
		ex44.sequence(as.map(a => Try(a.toInt)))

	import ex43.map2
	def traverse[T, R](xs: List[T])(f: T => Option[R]): Option[List[R]] = {
		xs match {
			case x :: xrest => map2(f(x), traverse(xrest)(f))(_ :: _)
			case Nil => Some(Nil)
		}
	}
	def parseInts(as: List[String]): Option[List[Int]] =
		traverse(as)(a => Try(a.toInt))

	def sequence[T](xs: List[Option[T]]): Option[List[T]] =
		traverse(xs)(x => x)

	def testParse {
		assert(parseIntsWithSeq(List("1","2")) == Some(List(1,2)))
		assert(parseIntsWithSeq(List("1","b")) == None)
		assert(parseInts(List("1","2")) == Some(List(1,2)))
		assert(parseInts(List("1","b")) == None)
	}

	def test {
		testParse
		assert(sequence(Some(2) :: Some(1) :: Nil) == Some(2 :: 1 :: Nil))
		assert(sequence(Some(2) :: None :: Nil) == None)
		assert(sequence(Nil) == Some(Nil))
	}
}
ex45.test


// this is a *disjoint* union of two possibilities.
// The "Right" value is the desirable output
// the Left is the "error" outcome

sealed trait Either[+E, +R] {
	def map[R2](f: R => R2): Either[E, R2]
	// if we are already fail we'll be returning an Either[E, R2]
	// which we must be able to consider as an Either[E2, R2]
	// so we need E <: E2
	def flatMap[E2 >: E, R2](f: R => Either[E2, R2]): Either[E2, R2]
	// we need Either[E, R] <: Either[E2, R2], which requires E <: E2, R <: R2
	def orElse[E2 >: E, R2 >: R](alternative: => Either[E2, R2]): Either[E2, R2]

	// when you request a.map2(b), the compiler has
	// the freedom to choose E3. This means that b could come in with a type
	// E2 originally. as long as it can be cast into an E3  and that E3 also
	// be a parent of a, that is the version of map that will be used.
	def map2[E3 >: E, R2, R3](b: Either[E3, R2])
							(f: (R, R2) => R3): Either[E3, R3]
}
case class Left[+E](value: E) extends Either[E, Nothing] {
	def map[R2](f: Nothing => R2): Either[E, Nothing] = this
	def flatMap[E2 >: E, R2](f: Nothing => Either[E2, R2]): Either[E2, R2] = Left(value)
	def orElse[E2 >: E, R2](alternative: => Either[E2, R2]): Either[E2, R2] = 
		alternative
	def map2[E3 >: E, R2, R3](b: Either[E3, R2])
					(f: (Nothing, R2) => R3): Either[E3, R3] = this
}
case class Right[+R](value: R) extends Either[Nothing, R] {
	def map[R2](f: R => R2): Either[Nothing, R2] = Right(f(value))
	def flatMap[E2, R2](f: R => Either[E2, R2]): Either[E2, R2] = f(value)
	def orElse[E2, R2 >: R](alternative: => Either[E2, R2]): Either[E2, R2] =
		this
	def map2[E3, R2, R3](b: Either[E3, R2])
						(f: (R, R2) => R3): Either[E3, R3] = b.map(f(value, _))
}

object ex46 {
	def safeInv(x: Int): Either[String, Int] = {
		if (x == 0) Left("divide by zero")
		else Right(1/x)
	}
	def test {
		val intValue: Either[String, Int] = Right(3)
		val intError: Either[String, Int] = Left("error")

		assert(intValue.map(2 * _) == Right(6))
		assert(intError.map(2 * _) == Left("error"))

		val zeroValue: Either[String, Int] = Right(0)

		assert(intValue.flatMap(safeInv(_)) == Right(0))
		assert(intError.flatMap(safeInv(_)) == Left("error"))
		assert(zeroValue.flatMap(safeInv(_)) == Left("divide by zero"))

		assert(intError.orElse(intValue) == intValue)
		assert(intValue.orElse(intError) == intValue)
		assert(intError.orElse(zeroValue.flatMap(safeInv(_))) == Left("divide by zero"))

		assert((intValue map2 intValue)(_ + _) == Right(6))
		assert((intError map2 intValue)(_ + _) == Left("error"))
		assert((intValue map2 intError)(_ + _) == Left("error"))
	}
}
ex46.test