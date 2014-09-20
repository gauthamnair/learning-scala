



abstract class Element {
    def height: Int
    def width: Int
}

    // def above(that: Element): Element
    // def beside(that: Element): Element


def elem(s: String): Element = ???

class ArrayElement(val contents: Array[String]) extends Element {
    def height = contents.length
    def width = if (height == 0) 0 else contents(0).length
}



// use case

def test() {
    val ae = new ArrayElement(Array("cat", "dog"))
    assert(ae.height == 2)
    assert(ae.width == 3)

    // val column1 = elem("hello") above elem("***")
    // val column2 = elem("***") above elem("world")
    // println(column1 beside column2)
}

test()