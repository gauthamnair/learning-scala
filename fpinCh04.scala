

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
