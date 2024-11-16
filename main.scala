class Reference(val varName: String, var value: Option[Int]):
  override def toString(): String = s"Reference($varName, $value)"

case class Node(
    value: String,
    left: Option[Node],
    right: Option[Node],
    ref: Option[Reference] = None
)

@main def main(): Unit =
  println("Hello, world!")
  val stringList = List("-", "*", "x", "x", "+", "x", "x")
  val (node, ls, refs) = tree(stringList)
  print(node)
  println()
  println(ls)

  println(refs.length)
  refs.foreach(ref => println(s"${ref.varName} = ${ref.value}"))
  // refs.foreach(ref => if ref.varName == "z" then ref.value = Some(5))
  // refs.foreach(ref => if ref.varName == "w" then ref.value = Some(3))
  refs.foreach(ref => if ref.varName == "x" then ref.value = Some(2))
  // refs.foreach(ref => if ref.varName == "y" then ref.value = Some(1))

  print(node)

def tree(
    list: List[String],
    op: Option[String] = None,
    refs: List[Reference] = Nil
): Tuple3[Node, List[String], List[Reference]] =
  op match
    case Some(op) =>
      println(s"Operator is: $op")
      val (rightTree, usedStrings, actualRefs) = tree(list)

      (
        Node("@", Some(Node(op, None, None)), Some(rightTree)),
        op :: usedStrings,
        refs ::: actualRefs
      )
    case None =>
      list match
        case head :: tail if Set("+", "-", "*", "/").contains(head) =>
          println(s"Found operator: $head")
          val (leftTree, leftUsedStrings, leftRefs) = tree(tail, Some(head))

          println(leftUsedStrings)

          // val listWithoutUsed = list.dropWhile(leftUsedStrings.contains)
          val listWithoutUsed = leftUsedStrings.foldLeft(list)((acc, element) =>
            if acc.head == element then acc.tail else acc
          )

          println(listWithoutUsed)
          val (rightTree, rightUsedStrings, rightRefs) = tree(listWithoutUsed)
          println(rightUsedStrings)

          (
            Node("@", Some(leftTree), Some(rightTree)),
            leftUsedStrings ::: rightUsedStrings,
            leftRefs ::: rightRefs
          )

        case _ =>
          list.headOption match
            case Some(head) =>
              println(s"Found leaf: $head")

              val ref = Reference(head, None)

              (
                Node(head, None, None, Some(ref)),
                head :: Nil,
                ref :: refs
              )
            case None =>
              println("No more elements")
              (Node("", None, None), Nil, Nil)
          end match
      end match
  end match
end tree

def printTree(node: Node): Unit =
  println(s"value: ${node.value}")
  node.left match
    case Some(left) =>
      println("left:")
      printTree(left)
    case None =>
      println("left: None")
  end match

  node.right match
    case Some(right) =>
      println("right:")
      printTree(right)
    case None =>
      println("right: None")
  end match
end printTree
