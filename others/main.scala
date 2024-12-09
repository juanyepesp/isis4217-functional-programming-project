class Reference(val varName: String, var value: Option[Int]):
  override def toString(): String = s"Reference($varName, $value)"

case class Node(
    value: String,
    left: Option[Node] = None,
    right: Option[Node] = None,
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
  refs.foreach(ref => if ref.varName == "x" then ref.value = Some(5))
  // refs.foreach(ref => if ref.varName == "y" then ref.value = Some(1))

  // print(node)
  // printTree(node)
  println()
  print(evalTree(node))

def tree(
    tokens: List[String],
    op: Option[String] = None
): Tuple3[Node, List[String], List[Reference]] =
  op match
    case Some(op) =>
      val leftTree = Node(op)
      val (rightTree, usedStrings, incomingRefs) = tree(tokens)

      (
        Node("@", Some(leftTree), Some(rightTree)),
        op :: usedStrings,
        incomingRefs
      )
    case None =>
      tokens match
        case head :: tail if Set("+", "-", "*", "/").contains(head) =>
          val (leftTree, leftUsedTokens, leftRefs) = tree(tail, Some(head))

          val unusedRemainingTokens =
            leftUsedTokens.foldLeft(tokens)((acc, element) =>
              if acc.head == element then acc.tail else acc
            )

          val (rightTree, rightUsedTokens, rightRefs) = tree(
            unusedRemainingTokens
          )

          (
            Node("@", Some(leftTree), Some(rightTree)),
            leftUsedTokens ::: rightUsedTokens,
            leftRefs ::: rightRefs
          )

        case _ =>
          val head = tokens.head
          val ref = Reference(head, None)

          (
            Node(head, None, None, Some(ref)),
            head :: Nil,
            ref :: Nil
          )
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

def evalTree(node: Node): Tuple2[Option[Int], Option[String]] =
  node.value match
    case "@" =>
      val (leftValue, leftOp) = evalTree(node.left.get)
      val (rightValue, _) = evalTree(node.right.get)

      (leftValue, leftOp) match
        case (Some(value), Some(leftOp)) =>
          (Some(evalOp(value, rightValue.get, leftOp)), None)
        case (None, Some(op)) =>
          (rightValue, Some(op))
        case _ => (None, None)

    case "+" | "-" | "*" | "/" =>
      (None, Some(node.value))
    case _ =>
      node.ref match
        case Some(ref) => (ref.value, None)
        case None      => (None, None)

def evalOp(leftValue: Int, rightValue: Int, op: String): Int =
  println(s"leftValue: $leftValue, rightValue: $rightValue, op: $op")
  op match
    case "+" => leftValue + rightValue
    case "-" => leftValue - rightValue
    case "*" => leftValue * rightValue
    case "/" => leftValue / rightValue
