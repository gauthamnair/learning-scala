import scala.io.Source

object LongLines {
	def getFromFile(fname: String,
		width: Int) {
		val source = Source.fromFile(fname)
		for (line <- source.getLines)
			processLine(line, width)
	}

	private def processLine(line: String, 
		width: Int) {
		if (line.length > width)
			println(line.length.toString + 
				":" + line.trim)
	}
}

LongLines.getFromFile("Rational.scala", 40)


def processFile(filename: String, width: Int) {
	def processLine(line: String) {
		if (line.length > width)
			println(line.length.toString + 
				":" + line.trim)
	}

	val source = Source.fromFile(filename)
	for (line <- source.getLines) {
		processLine(line)
	}
}

processFile("Rational.scala", 40)

def prefixWithLengthAndTrim(x: String): String = 
	x.length.toString + "*:*" + x.trim

def onlyLongLines(stringSeq: Iterator[String], 
		minWidth: Int): Iterator[String] = 
	stringSeq.filter(_.length >= minWidth)

def processFile2(filename: String, width: Int) {
	val source = Source.fromFile(filename)
	val lines = source.getLines
	val onlyLong = onlyLongLines(lines, width)
	val lengthPrefixed = onlyLong.map(prefixWithLengthAndTrim _)
	lengthPrefixed.foreach(println)
}



processFile2("Rational.scala", 40)


def processFile3(filename: String, width: Int) {
	val source = Source.fromFile(filename)
	val lines = source.getLines
	val onlyLong = lines.filter(_.length >= width)
	val lengthPrefixed = onlyLong.map(
		(x:String) => x.length + " => " + x.trim)
	lengthPrefixed.foreach(println)
}

processFile3("Rational.scala", 40)


//  Partially applied functions:

def sum(a:Int, b:Int, c:Int): Int = a + b + c

assert(sum(1,2,3) == 6)

val addPlus4 = sum(4, _:Int, _:Int)
assert(addPlus4(2,3) == 9)

// scala captures variables as variables in closures, not values
// this is like javascript

var more:Int = 10
def addMore(x:Int): Int = x + more

assert(addMore(3) == 13)

more = 20

assert(addMore(3) == 23)

// but if passed in as an argument to a functional:

def makeIncreaser(amount: Int) = (x: Int) => x + amount

val inc10 = makeIncreaser(10)
assert(inc10(3) == 13)

val incrByMore = makeIncreaser(more)

assert(incrByMore(3) == 23)

more = 30

assert(incrByMore(3) == 23)


// Let's see what happens if we have lazy
// evaluation of the amount

def lazyIncreaser(amount: => Int) = (x: Int) => x + amount

val lzyIncr = lazyIncreaser(20)

assert(lzyIncr(3) == 23)

val lzyIncrByMore = lazyIncreaser(more)

assert(lzyIncrByMore(3) == 33)

more = 40 

// seems to reproduce the closure behavior

assert(lzyIncrByMore(3) == 43)