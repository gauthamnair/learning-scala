

object ListsModuleUsingFunctionTypes {
	sealed trait List[+T] 
	case object Nil extends List[Nothing]
	case class Cons[T](head: T, tail: List[T]) extends List[T]

	object List {
		def apply[T](xs: T*): List[T] = 
			xs.foldRight(Nil: List[T])(Cons(_, _))
		def fill[T](n: Int)(x: T): List[T] =
			if (n > 0) Cons(x, fill(n-1)(x)) else Nil: List[T]
	}

	type FoldLeft[T,R] = (List[T], R) => (((R, T) => R) => R)
	type FoldRight[T,R] = (List[T], R) => (((T, R) => R) => R)

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

	// how can one inject the dependency on foldLeft?
	object FromFoldLeft {
		def reverse[T](xs: List[T]) = 
			foldLeft(xs, Nil: List[T])(
				(reversed, next) => Cons(next, reversed))

		def foldRight[T,R](xs: List[T], rightMost: R)(op: (T, R) => R): R = {
			val reversed = reverse(xs)
			foldLeft(reversed, rightMost)((x,y) => op(y,x))
		}

		case class TraversalState[T](reversedItems: List[T], keepGoing: Boolean) {
			def withNewItem(x: T) = TraversalState(Cons(x, reversedItems), keepGoing)
			def withKeepGoing(flag: Boolean) = TraversalState(reversedItems, flag)
		}

		def takeWhile[T](xs: List[T])(satisfies: T => Boolean): List[T] = {
			val initialState = TraversalState(Nil: List[T], true)
			val (reversedList, _) = foldLeft(xs, initialState)(
				(item, traversalState) => 
					if (traversalState.keepGoing && satisfies(item)) 
						traversalState.withNewItem(item)
					else 
						traversalState.withKeepGoing(false)
				)
			reverse(traversalState.reversedItems)
		}

		def test {
			val xs = List(1,2,3)
			assert(reverse(xs) == List(3,2,1))
			val reconstructed = foldRight(xs, Nil: List[Int])(Cons(_,_))
			assert(reconstructed == xs)
		}
	}


	object FromFoldRight {
		import FromFoldLeft.foldRight
		def map[T,R](xs: List[T])(f: T => R): List[R] = 
			foldRight(xs, Nil: List[R])((x, rest) => Cons(f(x), rest))

		def append[T](xs: List[T], rest: List[T]): List[T] = 
			foldRight(xs, rest)(Cons(_,_))

		def flatMap[T,R](xs: List[T])(f: T => List[R]): List[R] = 
			foldRight(xs, Nil: List[R])((x, rest) => append(f(x), rest))





		def test {
			val xs = List(1,2,3)
			assert(map(xs)(x => 2*x) == List(2,4,6))
			val replicated = flatMap(xs)(x => List.fill(x)(x))
			assert(replicated == List(1,2,2,3,3,3))
		}
	}

	def zip[A,B](as: List[A], bs: List[B]): List[(A,B)] = 
		as match {
			case Cons(a1, arest) => 
				bs match {
					case Cons(b1, brest) => Cons((a1,b1), zip(arest,brest))
					case Nil => Nil
				}
			case Nil => Nil
		}

	object FromFlatMap {
		import FromFoldRight.flatMap
		def map[T,R](xs: List[T])(f: T => R): List[R] = 
			flatMap(xs)(x => List(f(x)))
		def zip[A,B](as: List[A], bs: List[B]): List[(A,B)] = ???
			// flatMap(as)(a => flatMap(bs)(b => (a,b)))
		def filter[T](xs: List[T])(satisfies: T => Boolean): List[T] = 
			flatMap(xs)(x => if (satisfies(x)) List(x) else Nil)
	}


	def test {
		FromFoldLeft.test
		FromFoldRight.test
	}
}

ListsModuleUsingFunctionTypes.test

object listsModule {

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