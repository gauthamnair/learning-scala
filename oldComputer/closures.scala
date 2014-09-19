import scala.io.Source

object LongLinesGrabber {
    def processFile(filename: String, width:Int) {
        val source = Source.fromFile(filename)
        val longLines = source.getLines.filter(_.length > width)
        longLines.foreach(x => println(filename + ": " + x.trim))
    }
}

def echo(args: String*) =
    for (arg <- args) println(arg)

def recursionBomb(x: Int): Int =
    if (x == 0) throw new Exception("boom!")
    else recursionBomb(x - 1) + 1

@tailrec
def tailRecBomb(x: Int): Int =
    if (x == 0) throw new Exception("boom!")
    else tailRecBomb(x - 1)