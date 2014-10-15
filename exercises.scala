

object ListTree {
	sealed trait Tree[+T]
	case object EmptyTree extends Tree[Nothing]
	case class NonEmptyTree[T](branches: List[Tree[T]]) extends Tree[T]
}

object Parsers {
	case class ParseResult(result: List[String], rest: List[String]) {
		override def toString: String = "Parse Result: " + 
			result.toString + "\nRemaining:" + rest.toString
	}

	// It should have been possible to define the Parser
	// as just a type alias for functions that go from
	// List[String] => ParseResult

	sealed trait Parser {
		def parse(x: List[String]): ParseResult
		def andThen(p2: Parser): Parser = SequenceParser(this, p2)
	}

	// What I am really trying to do here is a kind of fold

	case class SequenceParser(p1: Parser, p2: Parser) extends Parser {
		def parse(x: List[String]): ParseResult = {
			val ParseResult(firstResult, rest) = p1.parse(x)
			val ParseResult(secondResult, remaining) = p2.parse(rest)
			ParseResult(firstResult ++ secondResult, remaining)
		}
	}
	case object EndOfString extends Parser {
		def parse(x: List[String]): ParseResult = x match {
			case Nil => ParseResult(Nil, Nil)
			case _ => throw new Exception("Expected end of string")
		}
	}
	case object AnyToken extends Parser {
		def parse(x: List[String]): ParseResult = x match {
			case first :: rest => ParseResult(List("*"), rest)
			case Nil => throw new Exception("Expected a token")
		}
	}
	case class Token(token: String) extends Parser {
		def parse(x: List[String]): ParseResult = x match {
			case first :: rest => {
				if (first == token) ParseResult(List(token), rest)
				else throw new Exception("Expected token " + token)
			}
			case Nil => throw new Exception("Expected a token")
		}
	}

	def test {
		val xs = List("hello")
		val pr = AnyToken.parse(xs)
		println(pr)
		val pr2 = Token("hello").parse(xs)
		println(pr2)

		val pseq = Token("hello") andThen AnyToken andThen Token("bird") andThen EndOfString
		val pseqR = pseq.parse(List("hello", "bear", "bird"))
		println(pseqR)
	}
}
Parsers.test

