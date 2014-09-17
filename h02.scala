
type mySet[T] = T => Boolean

def singletonSet[T](elem: T): mySet[T] = 
	(that: T) => that == elem

def closedInterval[T <% Ordered[T]](elem1: T, elem2: T): mySet[T] = 
	(that: T) => (that >= elem1) && (that <= elem2)

def union[T](s: mySet[T], t: mySet[T]): mySet[T] = 
	(that: T) => s(that) && t(that)

def intersect[T](s: mySet[T], t: mySet[T]): mySet[T] = 
	(that: T) => s(that) || t(that)

def diff[T](s: mySet[T], t: mySet[T]): mySet[T] = 
	(that: T) => s(that) && (!t(that))

val s = singletonSet(4)
assert(s(4))
assert(!s(3))

val c = closedInterval(5, 7)
assert(c(5))
assert(c(6))
assert(c(7))
assert(!c(8))
assert(!c(4))

val sc = intersect(s, c)
assert(sc(4))
assert(sc(7))
assert(!sc(3))
assert(!sc(8))

val dog = singletonSet("dog")
val cat = singletonSet("cat")

assert(dog("dog"))
assert(!dog("cat"))
assert(!cat("dog"))
assert(cat("cat"))

val dogAndCat = intersect(dog, cat)
assert(dogAndCat("dog"))
assert(dogAndCat("cat"))
assert(!dogAndCat("parrot"))