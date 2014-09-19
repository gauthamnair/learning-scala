
def gcdLoop(x: Int, y:Int): Int = {
    var a = x
    var b = y
    while (a != 0){
        val oldA = a
        a = b % a
        b = oldA
    }
    b
}

def gcd(a: Int, b:Int): Int =
    if (a==0) b
    else gcd(b % a, a)

val filesHere = new java.io.File(".")

for (file <- filesHere.listFiles)
    println(file)

val scalaFiles = filesHere.listFiles().filter(_.getName.endsWith(".scala"))

for (file <- scalaFiles)
    println(file)

def fileLines(file: java.io.File) =
    scala.io.Source.fromFile(file).getLines.toList

def findInSource(subString: String) = {
    val directoryInfo = new java.io.File(".")
    val allFiles = directoryInfo.listFiles.filter(_.isFile)
    val scalaFiles = allFiles.filter(_.getName.endsWith("scala"))
    for (
        file <- scalaFiles;
        line <- fileLines(file)
        if line.trim.contains(subString)
    ) yield(file.getName, line.trim)
}

def findMate(ingredient: String): String = {
    ingredient match {
        case "salt" => "pepper"
        case "chips" => "salsa"
        case "eggs" => "bacon"
        case _ => "unknown"
    }
}