// Our fundamental assumption will be that our library
// admits absolutely no side effects.

object scratch {
	object nonParallel {
		def sum(ints: Seq[Int]): Int = 
			ints.foldLeft(0)(_ + _)
	}

	object listing71 {
		def sum(ints: IndexedSeq[Int]): Int =
			if (ints.size <= 1)
				ints.headOption.getOrElse(0)
			else {
				val (l, r) = ints.splitAt(ints.length/2)
				sum(l) + sum(r)
			}
		}

	object listing72 {
		trait Par[A]
		object Par {
			def unit[T](x: => T): Par[T] = ???
			def get[T](px: Par[T]): T = ???
		}
		def sum(xs: IndexedSeq[Int]): Int = 
			if (xs.size <= 1)
				xs.headOption.getOrElse(0)
			else {
				val (l,r) = xs.splitAt(xs.length / 2)
				val sumL: Par[Int] = Par.unit(sum(l))
				val sumR: Par[Int] = Par.unit(sum(r))
				Par.get(sumL) + Par.get(sumR)
			}
	}

	object primitivesJava {
		trait Runnable {def run: Unit}
		abstract class Thread(r: Runnable) {
			def start: Unit // starts run in new thread
			def join: Unit // blocks calling thread
		}
		// The problem with these above is that
		// they don't return a value! They
		// only work via side effects

		trait Callable[T]
		trait Future[T] {
			def get: T
		}
		abstract class ExecutorService {
			def submit[T](task: Callable[T]): Future[T]
		}
		// Nothing particularly wrong here, but these
		// futures are blocking, and still missing is
		// a way to compose them. 
	}

	object section712 {
		// In the Listing 72 example it becomes clear
		// that if you are required to return an int
		// from sum, then your computation won't 
		// really be parallel. Rather let sum return
		// a parallel calculation of type Int.
		
		trait Par[A]
		object Par {
			// it is not clear whether lazy is still needed
			def unit[T](x: T): Par[T] = ???
			def get[T](px: Par[T]): T = ???
			def map2[A,B,R](pa: Par[A], pb: Par[B])
							(op: (A,B) => R): Par[R] = ???
			// this function's job is to take
			// expressions that evaluate to a parallel process
			// and return
			def fork[T](x: => Par[T]): Par[T] = ???
			def lazyUnit[T](x: => T): Par[T] = fork(unit(x))

			// The thought now is whether evaluation
			// should begin when fork is called or
			// only upon the get.

			// The authors make the argument that 
			// if you want to control resources, it
			// would appear that whoever wants to "get"
			// the result should supply the resources
			// to do the calculation, rather than who
			// ever built up the calculation procedure.

			// In other words, instead of get we can think of:
			def run[T](x: Par[T]): T = ???
			// and Par is just a data structure that
			// describes a parallel computation,
			// without knowing how to implement it.
		}

		def sum(xs: IndexedSeq[Int]): Par[Int] =
			if (xs.length <= 1)
				Par.unit(xs.headOption.getOrElse(0))
			else {
				val (l, r) = xs.splitAt(xs.length / 2)
				Par.map2(sum(l), sum(r))(_ + _)
			}

		object withFork {
			def sum(xs: IndexedSeq[Int]): Par[Int] =
				if (xs.length <= 1)
					Par.unit(xs.headOption.getOrElse(0))
				else {
					val (l, r) = xs.splitAt(xs.length / 2)
					Par.map2(Par.fork(sum(l)), 
						Par.fork(sum(r)))(_ + _)
				}
		}
	}

	object ex71 {
		def test {
			// existence of Par.map2
		}
	}
}