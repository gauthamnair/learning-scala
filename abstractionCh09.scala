
trait FileMatcher {
	def filesEnding(query: String): Seq[java.io.File]
	def filesContaining(query: String): Seq[java.io.File]
}

object basicFileMatcher extends FileMatcher {
	private def filesHere = 
		(new java.io.File(".")).listFiles

	override def toString: String = "basic matcher"

	def filesEnding(query: String) = 
		for (file <- filesHere; 
			if file.getName.endsWith(query)
			) yield file

	def filesContaining(query: String) = 
		for (file <- filesHere;
			if file.getName.contains(query)
			) yield file
}

def test(fileMatcher: FileMatcher) {
	println(fileMatcher)

	println("--> Files ending with '.scala'")
	fileMatcher.filesEnding(".scala").foreach(println)

	println("--> Files containing 'Structs'")
	fileMatcher.filesContaining("Structs").foreach(println)	
}

test(basicFileMatcher)

object secondFileMatcher extends FileMatcher {
	private def filesHere = (new java.io.File(".")).listFiles

	override def toString = "Second matcher"

	def filesMatchingNameCriterion(
		nameQuery: (String) => Boolean) =
		for (file <- filesHere;
			if nameQuery(file.getName)
			) yield file

	def filesEnding(query: String) = 
		filesMatchingNameCriterion(_.endsWith(query))

	def filesContaining(query: String) =
		filesMatchingNameCriterion(_.contains(query))
}

test(secondFileMatcher)


def containsNeg(nums: Seq[Int]) = nums.exists(_ < 0)

assert(containsNeg(Array(0, -1, 2)))
assert(!containsNeg(Array(0, 1, 2)))

def containsOdd(nums: Seq[Int]) = nums.exists(_ % 2 == 1)

assert(containsOdd(Array(0, 2, 3, 4)))
assert(!containsNeg(Array(0, 2, 4, 6)))


//  Currying

def curriedAdd(x: Int)(y: Int): Int = x + y

assert(curriedAdd(4)(5) == 9)

val add4 = curriedAdd(4)(_)

assert(add4(5) == 9)

// creating contexts

import java.io.PrintWriter

def withPrintWriter(filename: String, op: PrintWriter => Unit) {
	val writer = new PrintWriter(filename)
	try {
		op(writer)
	} finally {
		writer.close()
	}
}

withPrintWriter("thefile", _.write("hello world!\n"))

def withCurriedPrintWriter(
		filename: String
	)(op: PrintWriter => Unit) {
		val writer = new PrintWriter(filename)
		try {
			op(writer)
		} finally {
			writer.close()
		}
	}

withCurriedPrintWriter("theDate") {
	writer => writer.println(new java.util.Date)
}

