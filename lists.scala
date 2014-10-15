
object listsModule {

	import scala.language.higherKinds

	sealed trait List[+T] 
	case object Nil extends List[Nothing]
	case class Cons[T](head: T, tail: List[T]) extends List[T]

	object List {
		def apply[T](xs: T*): List[T] = 
			xs.foldRight(Nil: List[T])(Cons(_, _))
	}

	trait RightFolderLike {
		def foldRight[T,R](xs: List[T], rightMost: R)(op: (T, R) => R): R
	}
	trait LeftFolderLike {
		def foldLeft[T,R](xs: List[T], leftMost: R)(op: (R, T) => R): R
	}

	object UnsafeRightFolder extends RightFolderLike {
		def foldRight[T,R](xs: List[T], rightMost: R)(op: (T, R) => R): R = {
			def foldStep(rest: List[T]): R = 
				rest match {
					case Cons(first, rest) => op(first, foldStep(rest))
					case Nil => rightMost
				}
			foldStep(xs)
		}
	}

	object SafeLeftFolder extends LeftFolderLike {
		def foldLeft[T,R](xs: List[T], leftMost: R)(op: (R, T) => R): R = {
			@annotation.tailrec
			def foldStep(remaining: List[T], accumulated: R): R = 
				remaining match {
					case Cons(first, rest) => 
						foldStep(rest, op(accumulated, first))
					case Nil => accumulated
				}
			foldStep(xs, leftMost)
		}

		def test {
			val xs = List(1,2,3)
			assert(foldLeft(xs, 0)(_ + _) == 6)
			println("SaveLeftLoader works")
		}
	}

	class LeftFolderOps(leftFolder: LeftFolderLike) extends RightFolderLike {
		def reverse[T](xs: List[T]) = 
			leftFolder.foldLeft(xs, Nil: List[T])(
				(reversedXs, nextElem) => Cons(nextElem, reversedXs))

		def foldRight[T,R](xs: List[T], rightMost: R)(op: (T, R) => R): R = {
			val reversed = reverse(xs)
			leftFolder.foldLeft(reversed, rightMost)((x,y) => op(y, x))
		}
		
		def test {
			val xs = List(1,2,3)
			assert(reverse(xs) == List(3,2,1))
			val reconstructed = foldRight(xs, Nil: List[Int])(Cons(_,_))
			assert(reconstructed == xs)
			println("LeftFolderOps work")
		}
	}

	object TraitBasedFolding {
		trait LeftFoldable[T] {
			def foldLeft[R](accum: R)(op: (R, T) => R): R
		}

		def listToLeftFoldable[T](xs: List[T]) = new LeftFoldable[T] {
			def foldLeft[R](zero: R)(op: (R, T) => R): R = {
				@annotation.tailrec
				def foldStep(accum: R)(remaining: List[T]): R = 
					remaining match {
						case Cons(first, rest) => foldStep(op(accum, first))(rest)
						case Nil => accum
					}
				foldStep(zero)(xs)
			}
		}

		def test {
			val xs = List(1,2,3)
			val lFoldable = listToLeftFoldable(xs)
			assert(lFoldable.foldLeft(0)(_ + _) == 1 + 2 + 3)
			println("trait based left-fold works")
		}
	}


	def test {
		val xs = List(1,2,3)
		println(xs)
		assert(xs == Cons(1, Cons(2, Cons(3, Nil))))
		SafeLeftFolder.test
		val leftFolderOps = new LeftFolderOps(SafeLeftFolder)
		leftFolderOps.test
		TraitBasedFolding.test
	}

}

listsModule.test