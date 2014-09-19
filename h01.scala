
def pascal(c: Int, r: Int): Int = {
	require(c >= 0)
	require(r >= 0)
	if (c > r) 0
	else {
		if ((c == 0) || (c == r)) 1
		else pascal(c-1, r-1) + pascal(c, r-1)
	}
}

import scala.annotation.tailrec

@tailrec
def balancer(chars:List[Char], numOpen:Int): Boolean = {
	if (numOpen < 0) false
	else chars match {
		case Nil => numOpen == 0
		case first::rest => first match {
			case '(' => balancer(rest, numOpen + 1)
			case ')' => balancer(rest, numOpen - 1)
			case _ => balancer(rest, numOpen)
		}
	}
}

def balance(expr: String): Boolean = {
	balancer(expr.toList, 0)
}

assert(balance(""))
assert(balance("abc def"))
assert(balance("()"))
assert(balance("(if (zero? x) max (/ 1 x))"))
assert(balance("I told him (that it’s not (yet) done)"))

assert(!balance("(if (zero? x max (/ 1 x))"))
assert(!balance(":-)"))
assert(!balance("())("))

def countChange(money: Int, coins: List[Int]): Int = {
	if (money < 0) 0
	else if (money == 0) 1
	else {
		val oneCoinLess = coins.map(x => countChange(money - x, coins))
		oneCoinLess.reduce(_ + _)
	}
}