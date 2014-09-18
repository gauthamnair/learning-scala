
trait FileMatcher {
	def filesEnding(query: String): Seq[java.io.File]
	def filesContaining(query: String): Seq[java.io.File]
}

object basicFileMatcher extends FileMatcher {
	private def filesHere = 
		(new java.io.File(".")).listFiles

	override def toString: String = "basic matcher"

	def filesEnding(query: String) = 
		for (file <- filesHere; 
			if file.getName.endsWith(query)
			) yield file

	def filesContaining(query: String) = 
		for (file <- filesHere;
			if file.getName.contains(query)
			) yield file
}

def test(fileMatcher: FileMatcher) {
	println(fileMatcher)

	println("--> Files ending with '.scala'")
	fileMatcher.filesEnding(".scala").foreach(println)

	println("--> Files containing 'Structs'")
	fileMatcher.filesContaining("Structs").foreach(println)	
}

test(basicFileMatcher)

object secondFileMatcher extends FileMatcher {
	private def filesHere = (new java.io.File(".")).listFiles

	override def toString = "Second matcher"

	def filesMatchingNameCriterion(
		nameQuery: (String) => Boolean) =
		for (file <- filesHere;
			if nameQuery(file.getName)
			) yield file

	def filesEnding(query: String) = 
		filesMatchingNameCriterion(_.endsWith(query))

	def filesContaining(query: String) =
		filesMatchingNameCriterion(_.contains(query))
}

test(secondFileMatcher)


