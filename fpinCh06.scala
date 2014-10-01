
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
	}

	def main(args: Array[String]) {
		ex61.test
		ex62.test
	}
}