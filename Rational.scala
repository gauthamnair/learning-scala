
import scala.annotation.tailrec
import scala.language.implicitConversions

@tailrec
def gcdNonNegative(a:Int, b:Int): Int = {
	if (a == 0) b 
	else {
		if (a >= b) gcdNonNegative(a - b, b)
		else gcdNonNegative(b - a, a)
	}
}

@tailrec
def gcd(a: Int, b:Int): Int = {
	if (a < 0) gcd(-a, b)
	else { if (b < 0) gcd(a, -b)
		else gcdNonNegative(a, b)
	}
}

assert(gcd(9, 12) == 3)
assert(gcd(1, 12) == 1)
assert(gcd(15, 30) == 15)

class Rational(n:Int, d:Int) extends Ordered[Rational] {
	require(d != 0)
	private val thedivisor = gcd(n, d)
	
	val num = n / thedivisor
	val den = d / thedivisor
	
	override def toString: String = num + "/" + den
	
	def +(that: Rational): Rational = 
		new Rational(num * that.den + that.num * den, 
			den * that.den)

	def *(that: Rational): Rational = 
		new Rational(num * that.num, den * that.den)

	def /(that: Rational): Rational = 
		new Rational(num * that.den, den * that.num)

	def compare(that: Rational): Int = 
		(this.num * that.den) - (that.num * this.den)

	override def equals(that: Any) = that match {
		case that: Rational => this.num == that.num && this.den == that.den
		case _ => false 
	}
}

object Rational {
	def apply(n:Int, d:Int): Rational = new Rational(n, d)
	def apply(n:Int): Rational = new Rational(n, 1)
}

implicit def Int2Rational(x:Int): Rational = Rational(x)



def runTests() {
	val oneHalf = Rational(2, 4)
	assert(oneHalf.toString == "1/2")
	assert(oneHalf.num == 1)
	assert(oneHalf.den == 2)

	assert(oneHalf == Rational(1,2))
	assert(oneHalf != Rational(1,3))
	assert(oneHalf != Rational(3,2))

	assert(Rational(1,3) + Rational(1, 6) == Rational(1,2))
	assert(Rational(2,3) * Rational(1, 2) == Rational(1, 3))
	assert(Rational(2,3) / Rational(3, 2) == Rational(4, 9))

	assert(Rational(1,2) > Rational(1,3))
	assert(Rational(1,2) < Rational(2,3))
	assert(Rational(1,2) <= Rational(1,2))
	assert(Rational(1,2) >= Rational(1,2))

	assert(Rational(3) == Rational(3,1))

	assert(Rational(1,1) + 1 == Rational(2))
	assert(1 + Rational(1,1) == Rational(2))

}

runTests()