
val filesHere = (new java.io.File(".")).listFiles


filesHere.filter(_.getName().endsWith(".scala"))
	.foreach(println)

for (i <- 1 to 5) println("Iteration" + i)

for (i <- 1 until 5) println("Until5" + i)

for (i <- (1 to 5).filter(_ % 2 == 0)) 
	println("Even" + i)

for (
	file <- filesHere 
	if !file.getName().startsWith("h0");
	if file.getName().endsWith(".scala")
)	println(file)


def fileLines(file: java.io.File) = 
	scala.io.Source.fromFile(file).getLines().toList

for (line <- fileLines(new java.io.File("./Rational.scala")))
	println(line)

def grep(pattern: String) =
	for (
		file <- filesHere
		if file.getName.endsWith(".scala");
		line <- fileLines(file)
		if line.trim.matches(pattern)
		) println(file + ":" + line.trim)

def scalaFiles = filesHere.filter(_.getName.endsWith(".scala"))



grep(".*gcd.*")

val totalLines = scalaFiles.map(fileLines _).map(_.length).sum
println(totalLines)

val totalFlatMapLines = scalaFiles.flatMap(fileLines _).length
println(totalFlatMapLines)

