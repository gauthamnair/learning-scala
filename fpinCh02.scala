

def factorial(n: Int): Int = {
	@annotation.tailrec
	def accumulateFactorial(x:Int, accum:Int): Int = {
		if (x <= 0) accum
		else accumulateFactorial(x - 1, x * accum)
	}
	accumulateFactorial(n, 1)
}

assert(factorial(0) == 1)
assert(factorial(1) == 1)
assert(factorial(2) == 2)
assert(factorial(3) == 6)
assert(factorial(4) == 24)
assert(factorial(-2) == 1)

def findFirst[T](x: Array[T])(satisfiedWith: T => Boolean): Int = {
	@annotation.tailrec
	def findStartingAt(n:Int): Int = {
		if (n >= x.length) -1 
		else if (satisfiedWith(x(n))) n
		else findStartingAt(n + 1)
	}
	findStartingAt(0)
}

def findFirstExactlyMatching[T](as: Array[T], key: T): Int = 
	findFirst(as)(_ == key)

object testFindFirst {
	def test  {
		assert(findFirstExactlyMatching(Array(10,20,30), 30) == 2)
		assert(findFirstExactlyMatching(Array(10,20,30), 20) == 1)
		assert(findFirstExactlyMatching(Array(10,20,30), 40) == -1)

		assert(findFirst(Array(1,3,5,8,9,11))(_ % 2 == 0) == 3)
	}
}

testFindFirst.test

def isSorted[T](x: Array[T])(areInOrder: (T, T) => Boolean): Boolean = {
	@annotation.tailrec
	def sortedFromHereOn(i:Int): Boolean = {
		if (i + 1 >= x.length) true
		else {
			if (areInOrder(x(i), x(i+1))) sortedFromHereOn(i + 1)
			else false
		}
	}
	sortedFromHereOn(0)
}

object testIsSorted {
	def test {
		assert(isSorted(Array(1,2))(_ <= _))
		assert(!isSorted(Array(2,1))(_ <= _))
		assert(isSorted(Array("a","b","c"))(_ <= _))
		assert(!isSorted(Array("a","c","b"))(_ <= _))
	}
}
testIsSorted.test


def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
	(a:A) => ((b:B) => f(a,b))

val adder: Int => (Int => Int) = curry(_ + _)

object testCurry {
	def test {
		assert(adder(4)(5) == 9)
	}
}
testCurry.test


def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A) => f(g(a))

object testCompose {
	def test {
		val addOne: Int => Int = _ + 1
		val doubleValue: Int => Int = _ * 2
		val addOneAndDouble = compose(doubleValue, addOne)
		assert(addOneAndDouble(1) == 4)
		
	}
}
testCompose.test

