import java.nio.file.Path
import java.security.MessageDigest

sealed abstract class MerkleTree {
  def label: String
  def hash: Either[String, Array[Byte]]
  lazy val hashString: Either[String, String] = hash.map { hash =>
    val bytes = hash
    val sb = new StringBuilder()
    var i = 0
    while (i < bytes.length) {
      val hex = Integer.toHexString(0xff & bytes(i))
      if (hex.length() == 1)
        sb.append('0')
      sb.append(hex)
      i += 1
    }

    sb.toString()
  }

  def render(colors: Boolean = true): String = {

    def red(s: String) = if (colors) Console.RED + s + Console.RESET else s
    def bold(s: String) = if (colors) Console.BOLD + s + Console.RESET else s
    def yellow(s: String) =
      if (colors) Console.YELLOW + s + Console.RESET else s

    def go(
        nodes: List[MerkleTree],
        level: Int,
        result: Vector[String]
    ): Vector[String] = {
      val tab = if (level != 0) "└" + ("─" * level) + " " else ""

      nodes match {
        case Nil                        => result
        case (l: MerkleTree.Leaf) :: tl =>
          val toBytesLabel = l.toBytes.label
            .map(l => yellow(l + " "))
            .getOrElse("")

          val isError = l.hashString.isLeft
          val hashStrRaw =
            l.hashString.fold(identity, identity).grouped(64)

          def err(l: String) = if (isError && colors) red(l) else l

          val rd = l.toBytes.renderData(l.data)

          val lines = hashStrRaw.toVector.zipWithIndex.map { case (e, i) =>
            if (i == 0)
              Seq(
                err(e.padTo(64, ' ')),
                " | ",
                tab,
                bold(label + ": "),
                toBytesLabel,
                if (label == rd) "" else rd
              ).mkString
            else
              err(e.padTo(64, ' ')) + " | "
          }
          go(
            tl,
            level,
            result ++ lines
          )
        case (l: MerkleTree.Node) :: tl =>
          val isError = l.hashString.isLeft
          val hashStrRaw =
            l.hashString.fold(identity, identity).grouped(64)

          def err(l: String) = if (isError && colors) red(l) else l
          val lines = hashStrRaw.toVector.zipWithIndex.map { case (e, i) =>
            if (i == 0)
              err(e.padTo(64, ' ')) + " | " + tab + l.label
            else
              err(e.padTo(64, ' ')) + " | "
          }

          go(l.subtrees, level + 1, result ++ lines) ++ go(
            tl,
            level,
            Vector.empty
          )

      }

    }

    go(List(this), 0, Vector.empty).mkString("\n")
  }
}

object MerkleTree {
  val digest = MessageDigest.getInstance("SHA-256")

  def sha(data: Array[Byte]) = {
    try {
      digest.update(data)
      digest.digest()
    } finally {
      digest.reset()
    }
  }

  def serialise(
      t: MerkleTree,
      toBytesID: ToBytes[Any] => String = s =>
        ToBytes.toStringID(s).getOrElse(sys.error(s"Unknown ToBytes id: $s"))
  ): Vector[String] = {
    def go(tree: MerkleTree, level: Int): Vector[String] = {

      tree match {
        case n: Node =>
          import n.*
          val idLabel = " " * level + label
          Seq(idLabel, "n", tree.hashString.fold(identity, identity))
            .mkString("\t") +: subtrees.toVector.flatMap(go(_, level + 1))
        case l: Leaf =>
          import l.*
          val idLabel = " " * level + label
          Vector(
            Seq(
              idLabel,
              "l",
              toBytesID(toBytes),
              toBytes.renderData(data),
              tree.hashString.fold(identity, identity)
            )
              .mkString("\t")
          )

      }
    }

    "SHA-256" +: go(t, 0)
  }

  def read(
      lines: Vector[String],
      fromBytesId: String => ToBytes[Any] = s =>
        ToBytes.fromStringID(s).getOrElse(sys.error(s"Unknown ToBytes id: $s"))
  ): Either[String, MerkleTree] = {
    val (hasher, rest) = (lines.head, lines.tail)

    def hexToBytes(hex: String): Array[Byte] = {
      hex.grouped(2).map(s => Integer.parseInt(s, 16).toByte).toArray
    }

    def go(t: Vector[String], level: Int): Either[String, List[MerkleTree]] = {
      val indent = " " * level

      t.headOption match {
        case Some(line) =>
          val parts = line.split("\t").toList
          if (parts.length == 3) {
            val label = parts(0).stripPrefix(indent)
            val serialisedHash = parts(2)
            val childrenLines = t.tail.takeWhile(_.startsWith(indent + " "))
            for {
              children <- go(childrenLines, level + 1)
              node = new Node(label, children) {
                override lazy val hash = Right(hexToBytes(serialisedHash))
                override lazy val hashString = Right(serialisedHash)
              }
              rest <- go(
                t.tail.drop(childrenLines.length),
                level
              )
            } yield node :: rest
            // Node(label, go(childrenLines, level + 1)) ::
          } else if (parts.length == 5) { // leaf
            val label = parts(0).stripPrefix(indent)
            val toBytes = fromBytesId(parts(2))
            val stringData = parts(3)
            val serialisedData = toBytes.serialiseData(stringData)
            val serialisedHash = parts(4)

            go(t.tail, level).map { l =>
              new Leaf(label, serialisedData, toBytes) {
                override lazy val hash = Right(hexToBytes(serialisedHash))
                override lazy val hashString = Right(serialisedHash)
              } :: l
            }
          } else { // invalid
            Left(s"cannot parse line $line")
          } // node
        case None => Right(Nil)
      }
    }

    go(rest, 0).flatMap {
      case Nil         => Right(new Node("<empty tree>", Nil))
      case head :: Nil => Right(head)
      case _           => Left("Invalid tree format")
    }
  }

  class Leaf(
      val label: String,
      val data: Array[Byte],
      val toBytes: ToBytes[Any]
  ) extends MerkleTree {
    lazy val hash = toBytes.produce(data).map(sha)

  }
  class Node(val label: String, val subtrees: List[MerkleTree])
      extends MerkleTree {
    lazy val hash: Either[String, Array[Byte]] =
      subtrees
        .map(_.hash)
        .foldLeft[Either[String, Array[Byte]]](Right(Array[Byte]())) {
          (acc, el) =>
            acc.flatMap(a => el.map(b => a ++ b))
        }
        .map(sha)

  }

  def nest[T](label: String)(fields: T => Seq[MerkleTree]) =
    (t: T) => new MerkleTree.Node(label, fields(t).toList)

  def string(label: String, s: String) =
    new Leaf(label, s.getBytes, ToBytes.Str)

  def strings(label: String, s: Seq[String]) =
    new Node(label, s.toList.map(i => string(i, i)))

  def sortedStrings(label: String, s: Seq[String]) =
    new Node(label, s.sorted.toList.map(i => string(i, i)))

  def path(label: String, p: Path, tb: ToBytes[Path]) =
    new Leaf(label, p.toAbsolutePath().toString.getBytes, tb)

  def bool(label: String, value: Boolean) =
    string(label, value.toString)

  def sortedPaths(label: String, s: Seq[Path], tb: ToBytes[Path]) =
    new Node(
      label,
      s.sorted.toList.map(i =>
        new Leaf(
          i.toAbsolutePath().toString,
          i.toAbsolutePath().toString.getBytes,
          tb
        )
      )
    )
}
