
class Thing(val data: Int) {
    override def toString = data.toString
}

assert((new Thing(3)).toString == "3")

class Refined(
    data: Int, 
    val moredata:Int
    ) extends Thing(data) {

    override def toString = moredata.toString
    def anotherString: String = "extra"
}

assert((new Refined(4, 5)).toString == "5")
assert((new Refined(4, 5)).anotherString == "extra" )

val refinedStuff: Array[Refined] = Array(new Refined(1,2), new Refined(3, 4))

assert(refinedStuff(0).toString == "2")
assert(refinedStuff(0).anotherString == "extra")
assert(refinedStuff(1).toString == "4")
assert(refinedStuff(1).anotherString == "extra")

val stuff: Array[Thing] = Array(new Thing(1), new Thing(3))

assert(stuff(0).toString == "1")
assert(stuff(1).toString == "3")


val mixed: Array[Thing] = Array(new Thing(1), new Refined(3,4))

assert(mixed(0).toString == "1")
assert(mixed(1).toString == "4")

// This fails if you try to do it:
// mixed(1).anotherString
