
object foo {
	trait Gettable[T] {
		def get: T
	}
	case class Box[T](item: T) extends Gettable[T] {
		def get = item
	}
	object bar {
		def makeBox[T](item: T) = Box(item)
	}

	type MakeBox[T] = T => Box[T]

	val makeBoxedInt: Int => Box[Int] = Box(_)
	val makeBoxedInt2: MakeBox[Int] = Box(_)
	// this one does not compile
	// val makeBoxed[T]: T => Box[T] = Box(_)

	def makeBoxed[T]: T => Box[T] = Box(_)
	// if you put makeBoxed in the REPL you get:
	// Nothing => foo.Box[Nothing] = <function1>
	def makeBoxed2[T]: MakeBox[T] = Box(_)


	object ByClosure {
		object Boxer {
			def make[T]: T => Box[T] = makeBoxed
		}
	}

	object ByInjection {
		// fails, noting that existential
		// type import is required
		// case class Boxer(makeBox: MakeBox[_])

		case class Boxer[T](makeBox: MakeBox[T]) {
			def make(item: T): Box[T] = makeBox(item)
		}
		val intBoxer: Boxer[Int] = Boxer(makeBoxed)
	}
}