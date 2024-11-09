case class Node(value: String, left: Option[Node], right: Option[Node])

@main def main(): Unit =
  println("Hello, world!")
  val stringList = List("-", "*", "z", "y", "+", "w", "x")
  val (node, ls) = tree(stringList, None)
  print(node, ls)
  // println()
  // printTree(node)

def tree(
    list: List[String],
    op: Option[String]
): Tuple2[Node, List[String]] =
  op match
    case Some(op) =>
      println(s"Operator is: $op")
      val (rightTree, usedStrings) = tree(list, None)
      (
        Node("@", Some(Node(op, None, None)), Some(rightTree)),
        op :: usedStrings
      )
    case None =>
      list match
        case head :: tail if Set("+", "-", "*", "/").contains(head) =>
          println(s"Found operator: $head")
          val (leftTree, leftUsedStrings) =
            tree(tail, Some(head))

          println(leftUsedStrings)

          val listWithoutUsed = list.dropWhile(leftUsedStrings.contains)
          println(listWithoutUsed)
          val (rightTree, rightUsedStrings) = tree(listWithoutUsed, None)
          println(rightUsedStrings)
          (
            Node("@", Some(leftTree), Some(rightTree)),
            leftUsedStrings ::: rightUsedStrings
          )

        case _ =>
          list.headOption match
            case Some(head) =>
              println(s"Found leaf: $head")
              (Node(head, None, None), head :: Nil)
            case None =>
              println("No more elements")
              (Node("", None, None), Nil)
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

// (
//   Node(
//     @,
//     Some(
//       Node(
//         @,
//         Some(
//           Node(
//             -,
//             None,
//             None
//           )
//         ),
//         Some(
//           Node(
//             @,
//             Some(
//               Node(
//                 @,
//                 Some(
//                   Node(
//                     *,
//                     None,
//                     None
//                   )
//                 ),
//                 Some(
//                   Node(
//                     z,
//                     None,
//                     None
//                   )
//                 )
//               )
//             ),
//             Some(
//               Node(
//                 z,
//                 None,
//                 None
//               )
//             )
//           )
//         )
//       )
//     ),
//     Some(
//       Node(
//         z,
//         None,
//         None
//       )
//     )
//   ),
//   List(-, *)
// )
