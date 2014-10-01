
object ch06state {
	case class StateStep[S, +T](run: S => (T, S)){
		def map[R](f: T => R): StateStep[S, R] =
			StateStep(state => {
				val (result, newState) = run(state)
				(f(result), newState) 
				})
		def flatMap[R](f: T => StateStep[S, R]): StateStep[S, R] = 
			StateStep(state => {
				val (result, newState) = run(state)
				f(result).run(newState)
				})
		def map2[B, R](bstep: StateStep[S, B])
					(op: (T, B) => R): StateStep[S, R] = 
			flatMap(x => bstep.map(b => op(x,b)))
	}
	object StateStep {
		def unit[S, T](x: T): StateStep[S, T] = 
			StateStep(step => (x, step))

		@annotation.tailrec
		def seqStepper[S, T](
			results: List[T], 
			stepsLeft: List[StateStep[S,T]], 
			state: S): (List[T], S) = 
			stepsLeft match {
				case step :: moreSteps => {
					val (aResult, nextState) = step.run(state)
					seqStepper(aResult :: results, moreSteps, nextState)
				}
				case Nil => (results.reverse, state)
			}
		
		def sequence[S, T](steps: List[StateStep[S, T]]): StateStep[S, List[T]] = 
			StateStep(state => seqStepper(Nil: List[T], steps, state))
	}

	object ex610 {
		def testUnit {
			val simpleS: StateStep[Any, Int] = StateStep.unit(1)
			val (x, newState) = simpleS.run("anyState")
			assert(newState == "anyState")
			assert(x == 1)
		}
		val counter: StateStep[Int, Int] = StateStep(i => (i, i+1))
		def testMap {
			val (x, newState) = counter.run(2)
			assert(x == 2)
			val (y, sameState) = counter.map(2*_).run(2)
			assert(y == 4)
		}
		def testFlatMap {
			val simCounter = counter.flatMap(StateStep.unit(_))
			val (x, newState) = counter.run(2)
			assert(x == 2)
			assert(newState == 3)
			val (y, sameState) = simCounter.run(2)
			assert(y == 2)
			assert(sameState == 3)
		}
		def testMap2 {
			val twoInARow: StateStep[Int, (Int, Int)] = 
				(counter map2 counter)((_, _))
			val ((x1, x2), s2) = twoInARow.run(2)
			assert(x1 == 2)
			assert(x2 == 3)
			assert(s2 == 4)
		}
		def testSequence {
			val threeInARow: StateStep[Int, List[Int]] = 
				StateStep.sequence(List.fill(3)(counter))
			val (xs, s2) = threeInARow.run(2)
			assert(xs == List(2,3,4))
			assert(s2 == 5)
		}
		def test {
			testUnit
			testMap
			testMap2
			testSequence
		}
	}

	def main(args: Array[String]) {
		ex610.test
	}
}