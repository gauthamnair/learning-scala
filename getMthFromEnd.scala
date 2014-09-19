
import scala.annotation.tailrec

@tailrec
def getMthFromEndWTwo[T](
	x: List[T], lagging: List[T], 
	m: Int, d: Int): Option[T] = {
	x match {
		case Nil => if (d == m) Some(lagging.head)
					else None
		case head::rest => 
			if (m == d) getMthFromEndWTwo(rest, lagging.tail, m, d)
			else getMthFromEndWTwo(rest, lagging, m, d+1)
	}
}

def getMthFromEnd[T](x: List[T], m: Int): Option[T] = 
	getMthFromEndWTwo(x, x, m, 0)

val x = List(1, 2, 3, 4, 5, 6, 7)

println(getMthFromEnd(x, 1))
println(getMthFromEnd(x, 2))
println(getMthFromEnd(x, 4))

val y = List("a","b", "c", "d", "e")

println(getMthFromEnd(y, 1))
println(getMthFromEnd(y, 2))
println(getMthFromEnd(y, 7))