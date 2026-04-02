package merkle

import MerkleTree.*

sealed trait DiffTree extends Product with Serializable

object DiffTree {
  sealed trait DiffAction extends Product with Serializable
  object DiffAction {
    case object Added extends DiffAction
    case class Modified(oldHash: String, newHash: String) extends DiffAction
    case object Removed extends DiffAction
    case object Same extends DiffAction
  }

  case class DNode(label: String, action: DiffAction, subtrees: List[DiffTree])
      extends DiffTree
  case class DLeaf(label: String, action: DiffAction) extends DiffTree

  def create(
      old: MerkleTree,
      recent: MerkleTree
  ): Either[String, DiffTree] = {
    def go(o: MerkleTree, n: MerkleTree): Either[String, DiffTree] = {
      (o, n) match {
        case (o: Node, n: Node) if o.hashString == n.hashString =>
          Right(
            DNode(
              o.label,
              DiffAction.Same,
              Nil
            )
          )
        case (o: Node, n: Node) if o.label == n.label =>
          val oldByLabel = o.subtrees.map(s => s.label -> s).toMap
          val newByLabel = n.subtrees.map(s => s.label -> s).toMap
          val added = List.newBuilder[MerkleTree]
          val removed = List.newBuilder[MerkleTree]
          val modified = List.newBuilder[(MerkleTree, MerkleTree)]
          val same = List.newBuilder[MerkleTree]

          o.subtrees.foreach { oldChild =>
            val inNewTree = newByLabel.get(oldChild.label)

            inNewTree match {
              case Some(newChild) =>
                if (newChild.hashString != oldChild.hashString) {
                  modified += oldChild -> newChild
                } else {
                  same += newChild
                }
              case None => removed += oldChild
            }
          }

          newByLabel.foreach { case (label, newChild) =>
            if (!oldByLabel.contains(label)) {
              added += newChild
            }
          }

          val mod = modified
            .result()
            .map { case (o, n) => go(o, n).map(List(_)) }
            .reduceOption { (l, r) =>
              for {
                d1 <- l
                d2 <- r
              } yield d1 ++ d2
            }
            .getOrElse(Right(Nil))

          mod.flatMap { modified =>
            Right(
              DNode(
                o.label,
                DiffAction.Modified(
                  o.hashString.getOrElse(""),
                  n.hashString.getOrElse("")
                ),
                added.result().map(s => DLeaf(s.label, DiffAction.Added)) ++
                  removed
                    .result()
                    .map(s => DLeaf(s.label, DiffAction.Removed)) ++
                  modified
              )
            )
          }

        case (o: Leaf, n: Leaf) if o.hashString == n.hashString =>
          Right(DLeaf(o.label, DiffAction.Same))
        case (o: Leaf, n: Leaf) =>
          Right(
            DLeaf(
              o.label,
              DiffAction.Modified(
                o.hashString.getOrElse(""),
                n.hashString.getOrElse("")
              )
            )
          )
        case _ => Left("hash trees diverged")
      }

    }
    go(old, recent)
  }

  def render(tree: DiffTree, colors: Boolean): Vector[String] = {
    def green(s: String) = if (colors) Console.GREEN + s + Console.RESET else ""
    def red(s: String) = if (colors) Console.RED + s + Console.RESET else ""
    def yellow(s: String) =
      if (colors) Console.YELLOW + s + Console.RESET else ""

    import DiffAction.*
    def indicator(action: DiffAction) = action match {
      case Added                      => yellow("+")
      case Modified(oldHash, newHash) => yellow("~")
      case Removed                    => red("-")
      case Same                       => green("=")
    }

    def go(
        t: List[DiffTree],
        level: Int,
        acc: Vector[String]
    ): Vector[String] = {
      val tab = if (level != 0) "└" + ("─" * level) + " " else ""
      t match {
        case h :: t =>
          h match {
            case DNode(label, action, subtrees) =>
              val line = tab + indicator(action) + " " + label
              val sub = go(subtrees, level + 1, Vector.empty)
              (line +: sub) ++ go(t, level, acc)

            case DLeaf(label, action) =>
              val line = tab + indicator(action) + " " + label

              line +: go(t, level, acc)
          }
        case Nil => acc
      }
    }

    go(List(tree), 0, Vector.empty)
  }
}
