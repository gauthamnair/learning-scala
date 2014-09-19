import scala.annotation.tailrec

@tailrec
def gcd(a:Int, b:Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
}

implicit def intToRational(x: Int):Rational = 
    new Rational(x)

class Rational(numer: Int, denom:Int){
    require(denom != 0)
    private val g = gcd(numer, denom)
    val n = numer / g
    val d = denom / g
    
    // Alt. constructors
    def this(n: Int) = this(n, 1)

    override def toString = n + "/" + d
    
    def + (that: Rational): Rational =
        new Rational(
            n * that.d + that.n * d, 
            d * that.d
        )

    def - (that: Rational): Rational =
        new Rational(
            n * that.d - that.n * d, 
            d * that.d
        )

    def + (i: Int): Rational = 
        new Rational(n + i * d, d)

    def - (i: Int): Rational = 
        new Rational(n - i * d, d)

    def * (that: Rational): Rational = 
        new Rational(n * that.n, d * that.d)

    def * (i: Int): Rational = 
        new Rational(n * i, d)

    def / (that: Rational): Rational = 
        new Rational(n * that.d, d * that.n)

    def / (i: Int): Rational = 
        new Rational(n, d * i)

    def > (that: Rational): Boolean =
        n * that.d > that.n * d

    def < (that: Rational): Boolean =
        n * that.d < that.n * d

    def == (that: Rational): Boolean =
        n == that.n && d == that.d

    def max(that: Rational): Rational =
        if (this > that) this else that
}

