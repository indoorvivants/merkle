package merkle

import MerkleTree.*
import merkle.DiffTree.DNode
import merkle.DiffTree.DLeaf

/** A [[DiffTree]] is a data structured produced by comparing two [[MerkleTree]]
  * instances with a similar structure. This tree does not necessarily cover
  * every node and leaf present in either of the original [[MerkleTree]]
  * instances – the focus is on the differences.
  */
sealed trait DiffTree extends Product with Serializable {
  def hasDifferences: Boolean = this match {
    case DNode(label, action, subtrees) =>
      action != DiffTree.DiffOutcome.Same
    case DLeaf(label, action) => action != DiffTree.DiffOutcome.Same
  }

  def render(colors: Boolean = true): Vector[String] =
    DiffTree.render(this, colors)
}

object DiffTree {
  sealed trait DiffOutcome extends Product with Serializable
  object DiffOutcome {
    case object Added extends DiffOutcome
    case class Modified(oldHash: String, newHash: String) extends DiffOutcome
    case object Removed extends DiffOutcome
    case object Same extends DiffOutcome
  }

  case class DNode(label: String, action: DiffOutcome, subtrees: List[DiffTree])
      extends DiffTree

  case class DLeaf(label: String, action: DiffOutcome) extends DiffTree

  /** Constructs a [[DiffTree]] from two [[MerkleTree]] instances. The trees
    * must have the same structure, with nodes and leaf labels being the same,
    * otherwise this function will produce divergence error.
    *
    * The resulting tree won't necessarily contain the same node structure as
    * the input trees. The focus is on showcasing differences, so the tree is
    * constructed lazily, accumulating differences as early as possible – early
    * meaning "as close to the root as possible"
    *
    * @param before
    *   the "before" version of the tree
    * @param after
    *   the "after" version of the tree
    * @return
    */
  def create(
      before: MerkleTree,
      after: MerkleTree
  ): Either[String, DiffTree] = {
    def go(o: MerkleTree, n: MerkleTree): Either[String, DiffTree] = {
      (o, n) match {
        case (o: Node, n: Node)
            if o.label == n.label && o.hashString == n.hashString =>
          Right(
            DNode(
              o.label,
              DiffOutcome.Same,
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
                DiffOutcome.Modified(
                  o.hashString.getOrElse(""),
                  n.hashString.getOrElse("")
                ),
                added.result().map(s => DLeaf(s.label, DiffOutcome.Added)) ++
                  removed
                    .result()
                    .map(s => DLeaf(s.label, DiffOutcome.Removed)) ++
                  modified
              )
            )
          }

        case (o: Leaf, n: Leaf)
            if o.label == n.label && o.hashString == n.hashString =>
          Right(DLeaf(o.label, DiffOutcome.Same))
        case (o: Leaf, n: Leaf) if o.label == n.label =>
          Right(
            DLeaf(
              o.label,
              DiffOutcome.Modified(
                o.hashString.getOrElse(""),
                n.hashString.getOrElse("")
              )
            )
          )
        case _ => Left("hash trees diverged")
      }

    }
    go(before, after)
  }

  /** Renders a [[DiffTree]] as a vector of strings, with optional color
    * highlighting. This is not a serialisation mechanism, the tree cannot be
    * read back from this representation.
    *
    * @param tree
    *   the tree to render
    * @param colors
    *   whether to use color highlighting
    * @return
    *   a vector of strings representing the rendered tree
    */
  def render(tree: DiffTree, colors: Boolean = true): Vector[String] = {
    def green(s: String) = if (colors) Console.GREEN + s + Console.RESET else ""
    def red(s: String) = if (colors) Console.RED + s + Console.RESET else ""
    def yellow(s: String) =
      if (colors) Console.YELLOW + s + Console.RESET else ""

    import DiffOutcome.*
    def indicator(action: DiffOutcome) = action match {
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
