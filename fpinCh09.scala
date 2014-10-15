

abstract class ParseError

// Think of this as writing a spec for a *module*
trait ParserModule[ParseError, Parser[+_]] {  
	thisParserModule => // weird naming of this particular parserModule
	def run[T](p: Parser[T])(input: String): Either[ParseError, T]
	def char(c: Char): Parser[Char]
	def string(s: String): Parser[String]
	def endOfInput: Parser[String] // the result should not be a string

	def anyChar: Parser[Char]

	def doubleChar: Parser[Char, Int] = anyChar.flatMap(c => char(c))
	def charOption(c: Char): Parser[Option[Char]] = 
		char(c).map(Some(_)) or unit(None: Option[Char])
	def countChar(c: Char): Parser[Int] = {
		def countedChars(countSoFar) = {
			charOption(c).flatMap({
				case Some(x) => countedChars(countSoFar + 1)
				case None => unit(countSoFar)
				})
		}
		countedChars(0)
	}
		

	def or[T](p1: Parser[T], p2: Parser[T]): Parser[T]
	def listOfN[T](n: Int, p: Parser[T]): Parser[List[T]]
	def map(p: Parser[T])(f: T => R): Parser[R]
	def map2(p1: Parser[A])(p2: Parser[B])(op: (A, B) => C): Parser[C]
	def flatMap(p: Parser[T])(f: T => Parser[R]): Parser[R]

	// running the unit parser on any input should return Right(x)
	def unit[T](x: T): Parser[T] 


	// the idea is that a Parser can be promoted
	// to a ParserWithOps when it is necessary 
	implicit def augmentWithOperators[T](
		p: Parser[T]): ParserWithOps[T] = ParserWithOps(p)

	case class ParserWithOps[A](p: Parser[A]) {
		def or[B >: A](p2: => Parser[B]): Parser[B] = thisParserModule.or(p.p2)
	}
}

class ParserSetTest(pm: ParserModule[_, _]) {
	def testRunChar {
		val testChars: List[Char] = ???
		testChars.foreach(
			c => pm.run(pm.char(c))(c.toString) == Right(c))
	}
	def testRunString {
		val testStrings: List[String] = ???
		testStrings.foreach(
			s => pm.run(pm.string(s))(s) == Right(s))
	}
}

object TrivalParserSet extends ParserModule[JsonParseError, JsonParser] {

}

object thoughts {

	trait Parser[T]

	def run[T](p: Parser[T])(input: String): Either[ParseError, T]

	def matchesAChar(c: Char): Parser[Char] = ???

	def matchesAString(s: String): Parser[String] = ???

	def or[T](p1: Parser[T], p2: Parser[T]): Parser[T] = ???

	def listOfN[T](n: Int, p: Parser[T]): Parser[List[T]] = ???

// Expect:
// or is associative:
// a or (b or c)  ==   (a or b) or c
// but it should not be commutative:
// a or b != b or a
// it should prefer to match a if it is possible to match a

// What about sequences of parsing?
// a andThen b means run should give Either[Error,List[Ra, Rb]]
// How about composing parsers
// map2(p1: Parser[R1])(p2: Parser[R2])(op: (R1, R2) => R): Parser[R]
// map(p: Parser[R])(f: R => R2): Parser[R2]


}