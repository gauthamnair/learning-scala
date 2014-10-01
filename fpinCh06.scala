
object ch06 {
	val rng = new scala.util.Random
	def rollDie(rng: scala.util.Random): Int = rng.nextInt(6) + 1
	
	trait RNG {
		def nextInt: (Int, RNG)
	}	

	case class SimpleRNG(seed: Long) extends RNG {
		def nextInt: (Int, RNG) = {
			val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
			val nextRNG = SimpleRNG(newSeed)
			val n = (newSeed >>> 16).toInt
			(n, nextRNG)
		}
	}

	def randomPair(rng: RNG): ((Int, Int), RNG) = {
		val (i1, rng2) = rng.nextInt
		val (i2, rng3) = rng2.nextInt
		((i1, i2), rng3)
	}

	// For the most part can just negate negative numbers
	// to turn them to positive. But..
	// 1. zero only happens once
	// 2. Int.MinValue has a bigger abs.value than Int.MaxValue
	// So we solve both problems by mapping Int.MinValue to 0.
	// Really should be adding this function to the trait.
	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (i, rng2) = rng.nextInt
		if (i == Int.MinValue) {
			(0, rng2)
		} else if (i < 0) {
			(-i, rng2)
		} else (i, rng2)
	}

	object ex61 {
		def test {
			val rng = SimpleRNG(42)
			val (i1, rng2) = nonNegativeInt(rng)
			val (i2, rng3) = nonNegativeInt(rng2)
			val (i3, rng4) = nonNegativeInt(rng3)
			assert(List(i1,i2,i3).forall(_ >= 0))
		}
	}

	def double(rng: RNG): (Double, RNG) = {
		val (i, rng2) = nonNegativeInt(rng)
		val x = i.toDouble / Int.MaxValue.toDouble
		(x, rng2)
	}

	object ex62 {
		def test {
			val rng = SimpleRNG(42)
			val (x1, rng2) = double(rng)
			val (x2, rng3) = double(rng2)
			val (x3, rng4) = double(rng3)
			assert(List(x1,x2,x3).forall(x => (x >= 0.0) && (x <= 1.0)))
		}
	}

	object ex63 {
		def get2Rands[A, B](rng: RNG, 
			fA: RNG => (A, RNG), 
			fB: RNG => (B, RNG)): ((A, B), RNG) = {
			val (a, rng2) = fA(rng)
			val (b, rng3) = fB(rng2)
			((a, b), rng3)
		}
		def intAndDouble(rng: RNG): ((Int, Double), RNG) = 
			get2Rands(rng, _.nextInt, double(_))
		def doubleAndInt(rng: RNG): ((Double, Int), RNG) = 
			get2Rands(rng, double(_), _.nextInt)
		def threeDoubles(rng: RNG): ((Double, Double, Double), RNG) = {
			val ((d1, d2), rng3) = get2Rands(rng, double(_), double(_))
			val (d3, rng4) = double(rng3)
			((d1, d2, d3), rng4)
		}
		def test {
			val rng = SimpleRNG(42)
			def testIntAndDouble {
				val ((i,x), rng2) = intAndDouble(rng)	
			}
			testIntAndDouble
			def testDoubleAndInt {
				val ((x,i), rng2) = doubleAndInt(rng)
			}
			testDoubleAndInt
			def testThreeDoubles {
				val ((x1, x2, x3), rng2) = threeDoubles(rng)
			}
			testThreeDoubles
		}
	}

	object ex64 {
		def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
			@annotation.tailrec
			def getNextInt(result: List[Int], rng: RNG, remaining: Int): (List[Int], RNG) = {
				if (remaining <= 0) (result, rng) else {
					val (i, nextRNG) = rng.nextInt
					getNextInt(i :: result, nextRNG, remaining - 1)
				}
			}
			getNextInt(Nil, rng, count)
		}
		def test {
			val rng = SimpleRNG(42)
			val (xs, rng2) = ints(4)(rng)
			assert(xs.length == 4)
		}
	}

	type Rand[+T] = RNG => (T, RNG)

	val int: Rand[Int] = _.nextInt

	// a pass-through
	def unit[T](a: T): Rand[T] = rng => (a, rng)

	def map[T, R](rand: Rand[T])(f: T => R): Rand[R] = 
		rng => {
			val (x, rng2) = rand(rng)
			(f(x), rng2)
		}

	def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

	object ex65 {
		def double: Rand[Double] = map(int)(i => {
			if (i == Int.MinValue) 0.0 else (i.abs.toDouble / Int.MaxValue.toDouble)
			})

		def test {
			val rng = SimpleRNG(42)
			val (x, rng2) = double(rng)
			assert(x <= 1.0)
			assert(x >= 0.0)
		}
	}

	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(op: (A, B) => C): Rand[C] = 
		rng => {
			val (a, rng2) = ra(rng)
			val (b, rng3) = rb(rng2)
			(op(a, b), rng3)
		}

	object ex66 {
		def test {
			val rng = SimpleRNG(42)
			val twoInts = map2(int, int)((_, _))
			val ((i, j), rng2) = twoInts(rng)
			val (i1, rng_1) = int(rng)
			val (i2, rng_2) = int(rng_1)
			assert(i == i1)
			assert(j == i2)
		}
	}

	def both[A, B](ra: Rand[A], rb:Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

	val randIntDouble: Rand[(Int, Double)] = both(int, double)
	val randDoubleInt: Rand[(Double, Int)] = both(double, int)

	@annotation.tailrec
	def randSeqStepper[T](results: List[T], rands: List[Rand[T]], rng: RNG): (List[T], RNG) =
		rands match {
			case r :: restOfRands => {
				val (x, newRNG) = r(rng)
				randSeqStepper(x :: results, restOfRands, newRNG)
			}
			case Nil => (results.reverse, rng)
		}
	def sequence[T](fs: List[Rand[T]]): Rand[List[T]] = 
		rng => randSeqStepper(Nil, fs, rng)

	object ex67 {
		def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))
		def testMixed {
			val threeMixedInts = sequence(List(int, nonNegativeInt, nonNegativeEven): List[Rand[Int]])
			val rng = SimpleRNG(42)
			val (xs, rngEnd) = threeMixedInts(rng)
			val (i1, rng2) = int(rng)
			val (i2, rng3) = nonNegativeInt(rng2)
			val (i3, rng4) = nonNegativeEven(rng3)
			assert(xs == List(i1, i2, i3))
		}
		def testInts {
			val threeInts = ints(3)
			val rng = SimpleRNG(42)
			val (xs, rngEnd) = threeInts(rng)
			val (i1, rng2) = int(rng)
			val (i2, rng3) = int(rng2)
			val (i3, rng4) = int(rng3)
			assert(xs == List(i1, i2, i3))
		}
		def test {
			testInts
			testMixed
		}
	}

	def nonNegativeLessThan(n: Int): Rand[Int] = {
		@annotation.tailrec
		def tryNonNegativeLessThan(rng: RNG): (Int, RNG) = {
			val (i, nextRNG) = nonNegativeInt(rng)
			if (i / n >= Int.MaxValue / n) {
				tryNonNegativeLessThan(nextRNG)
			} else (i % n, nextRNG)
		}
		rng => tryNonNegativeLessThan(rng)
	}

	def flatMap[T, R](rand: Rand[T])(f: T => Rand[R]): Rand[R] = 
		rng => {
			val (x, nextRNG) = rand(rng)
			val newRand = f(x)
			newRand(nextRNG)
		}
	
	object ex68 {
		def nonNegativeLessThan(n:Int): Rand[Int] = 
			flatMap(nonNegativeInt)(i => 
				if ((i / n) >= (Int.MaxValue / n)){
					nonNegativeLessThan(n)
				} else unit(i))
		def testInt {
			val intAgain = flatMap(int)(unit)
			val rng = SimpleRNG(42)
			val (i, rng2) = intAgain(rng)
			val (ip, rng2p) = int(rng)
			assert(i == ip)
			assert(rng2 == rng2p)
		}	
		def test {
			testInt
			val cap = Int.MaxValue / 2 + Int.MaxValue / 10
			val lessThanCap = nonNegativeLessThan(cap)
			val rng = SimpleRNG(42)
			val tenLessThans = sequence(List.fill(10)(lessThanCap))
			val (xs, rng2) = tenLessThans(rng)
			assert(xs.forall(_ < cap))
		}
	}

	object ex69 {
		def map[T,R](rand: Rand[T])(f: T => R): Rand[R] = 
			flatMap(rand)(x => unit(f(x)))
		def map2[A, B, C](ra: Rand[A], rb: Rand[B])(op: (A, B) => C): Rand[C] =
			flatMap(ra)(a => 
				flatMap(rb)(b => 
					unit(op(a,b))
					)
				)
		// Can't do this because flatMap is not defined as a method on Rand:
		// (but otherwise would work)
		// def map2[A, B, C](ra: Rand[A], rb: Rand[B])(op: (A, B) => C): Rand[C] = 
		// 	for (a <- ra; b <- rb) yield op(a, b)

		def test {
			val rng = SimpleRNG(42)
			val twoInts = map2(int, nonNegativeInt)(_ + _)
			val (theSum, rng2) = twoInts(rng)
			val (i1, rng_1) = int(rng)
			val (i2, rng_2) = nonNegativeInt(rng_1)
			assert(theSum == (i1 + i2))
		}
	}


	def main(args: Array[String]) {
		ex61.test
		ex62.test
		ex63.test
		ex64.test
		ex65.test
		ex66.test
		ex67.test
		ex68.test
		ex69.test
	}

}