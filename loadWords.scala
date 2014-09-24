
val wordsFile = scala.io.Source.fromFile("linuxwords.txt")
val words = wordsFile.getLines.toList

def thePositives: Stream

def preOrderTraverse[T](tree: Tree[T]): List[T] =
	tree match {
		case EmptyTree() => Nil
		case NonEmptyTree(key, leftTree, rightTree) => {
			(key :: preOrderTraverse(leftTree)) ::: preOrderTraverse(rightTree)
		}
	}