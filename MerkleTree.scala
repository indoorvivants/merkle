package merkle

import java.nio.file.Path
import java.security.MessageDigest
import java.security.NoSuchAlgorithmException
import scala.annotation.nowarn

/** A Merkle tree is a data structure which consists of leaves – that hash their
  * data contents according to user-provided procedure and data interpretation,
  * and nodes – which hash the hashes of their subtrees.
  *
  * This allows for fast comparison between two trees, as we only need to
  * compared the root node hashes
  */
sealed abstract class MerkleTree {

  /** Instance of the Hasher used to produce hashes
    *
    * @return
    */
  def hasher: Hasher

  /** Human-readable label for this node. Labels are important – they are
    * preserved in (de-)serialisation, are present in the rendering, and are
    * used for structural checks when constructing a [[DiffTree]]
    *
    * @return
    */
  def label: String

  /** Hash of this node contents. IMPORTANT: hash computation is lazy and
    * cached. The only reason this is a `def` and not a `lazy val` is that Scala
    * does not allow abstract lazy values
    *
    * @return
    */
  def hash: Either[String, Array[Byte]]

  /** Stringified hex representation of the hash of the data (see [[hash]])
    *
    * @return
    */
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

  /** Renders the tree as a list of strings.
    *
    * @param colors
    *   whether to use ASCII colours or not
    * @return
    */
  def render(colors: Boolean = true): Vector[String] =
    MerkleTree.render(this, colors)
}

object MerkleTree {

  /** Serialises the tree to a list of lines that can be saved to a file and
    * read back again using the [[deserialise]] method.
    *
    * IMPORTANT: serialisation will force the evaluation of all the hashes
    *
    * @param tree
    *   tree to serialise
    * @param toHashID
    *   function to convert a [[Hasher]] to a string ID (see
    *   [[Hasher.toStringID]])
    * @param toBytesID
    *   function to convert a [[ToBytes]] instance to a string ID (see
    *   [[ToBytes.toStringID]])
    * @return
    */
  def serialise(
      tree: MerkleTree,
      toHashID: Hasher => Option[String] = Hasher.toStringID(_),
      toBytesID: ToBytes[Any] => Option[String] = ToBytes.toStringID(_)
  ): Either[String, Vector[String]] = {
    def go(tree: MerkleTree, level: Int): Either[String, Vector[String]] = {
      tree match {
        case n: Node =>
          import n.*
          val idLabel = " " * level + label
          for {
            cur <- Right(
              Seq(idLabel, "n", tree.hashString.fold(identity, identity))
                .mkString("\t")
            )
            next <- subtrees.toVector.foldLeft[Either[String, Vector[String]]](
              Right(Vector.empty[String])
            ) { (acc, el) =>
              for {
                cur <- acc
                nest <- go(el, level + 1)
              } yield cur ++ nest
            }
          } yield cur +: next
        case l: Leaf =>
          val idLabel = " " * level + l.label

          for {
            tbID <- toBytesID(l.toBytes).toRight(
              s"No ID for ToBytes: ${l.toBytes}"
            )
          } yield Vector(
            Seq[String](
              idLabel,
              "l",
              tbID,
              l.toBytes.serialise(l.data),
              tree.hashString.fold(identity, identity)
            ).mkString("\t")
          )

      }
    }

    for {
      hashLine <- toHashID(tree.hasher).toRight(
        s"No hash ID for hasher ${tree.hasher}"
      )
      tree <- go(tree, 0)
    } yield hashLine +: tree
  }

  /** Deserialises the Merkle tree from a list of strings. IMPORTANT: the hashes
    * are read from the serialised format and cached in the tree – meaning the
    * tree will never recompute the hashes even if they depend on external
    * factors such as file system.
    *
    * This means an instance of the tree returned from this method is frozen in
    * time and is safe to compare to another tree of same structure constructed
    * over live data
    *
    * @param lines
    *   lines of the serialised tree (without line end markers)
    * @param fromBytesId
    *   function that produces an instance of ToBytes from its serialised ID
    *   (see [[ToBytes.fromStringID]])
    * @param fromHasherId
    *   function that produces an instance of Hasher from its serialised ID (see
    *   [[Hasher.fromStringID]])
    * @return
    */
  def deserialise(
      lines: Vector[String],
      fromBytesId: String => Option[ToBytes[Any]] = s =>
        ToBytes.fromStringID(s),
      fromHasherId: String => Option[Hasher] = s => Hasher.fromStringID(s)
  ): Either[String, MerkleTree] = {
    val (hasherID, rest) = (lines.head, lines.tail)

    def hexToBytes(hex: String): Array[Byte] = {
      hex.grouped(2).map(s => Integer.parseInt(s, 16).toByte).toArray
    }

    def go(
        hasher: Hasher,
        t: Vector[String],
        level: Int
    ): Either[String, List[MerkleTree]] = {
      val indent = " " * level

      t.headOption match {
        case Some(line) =>
          val parts = line.split("\t").toList
          if (parts.length == 3) {
            val label = parts(0).stripPrefix(indent)
            val serialisedHash = parts(2)
            val childrenLines = t.tail.takeWhile(_.startsWith(indent + " "))
            for {
              children <- go(hasher, childrenLines, level + 1)
              // IMPORTANT
              // We override the hashes in the nodes read from the file to make sure
              // they are not computed on real inputs – which is not an issue for strings, but
              // things like FileMtime will be incorrect
              node = new Node(hasher, label, children) {
                @nowarn
                override lazy val hash =
                  Right[String, Array[Byte]](
                    hexToBytes(serialisedHash)
                  )

                @nowarn
                override lazy val hashString =
                  Right[String, String](serialisedHash)
              }
              rest <- go(hasher, t.tail.drop(childrenLines.length), level)
            } yield node :: rest
          } else if (parts.length == 5) { // leaf
            val label = parts(0).stripPrefix(indent)
            val toBytes = fromBytesId(parts(2)).getOrElse {
              return Left(s"Unknown ToBytes id: ${parts(2)}")
            }

            val stringData = parts(3)
            val serialisedData = toBytes.deserialise(stringData)
            val serialisedHash = parts(4)

            for {
              tb <- fromBytesId(parts(2)).toRight(
                s"Unknown ToBytes id: ${parts(2)}"
              )
              rest <- go(hasher, t.tail, level).map { l =>
                new Leaf(hasher, label, serialisedData, toBytes) {
                  // IMPORTANT
                  // We override the hashes in the nodes read from the file to make sure
                  // they are not computed on real inputs – which is not an issue for strings, but
                  // things like FileMtime will be incorrect
                  @nowarn
                  override lazy val hash =
                    Right[String, Array[Byte]](hexToBytes(serialisedHash))

                  @nowarn
                  override lazy val hashString =
                    Right[String, String](serialisedHash)
                } :: l
              }
            } yield rest
          } else { // invalid
            Left(s"cannot parse line: $line")
          }
        case None => Right(Nil)
      }
    }

    val detectedHasher =
      fromHasherId(hasherID).toRight(s"Unknown Hasher id: $hasherID")

    for {
      hasher <- detectedHasher
      tree <- go(hasher, rest, 0)
      result <- tree match {
        case Nil         => Right(new Node(hasher, "<empty tree>", Nil))
        case head :: Nil => Right(head)
        case _           => Left("Invalid tree format")
      }
    } yield result
  }

  class Leaf private[merkle] (
      val hasher: Hasher,
      val label: String,
      val data: Array[Byte],
      val toBytes: ToBytes[Any]
  ) extends MerkleTree {
    lazy val hash = toBytes.hashableData(data).map(hasher.hash(_))

    override def toString: String =
      s"MerkleTree.Leaf($label, ${ToBytes.toStringID(toBytes).getOrElse("")} ${toBytes.serialise(data)})"

  }
  class Node private[merkle] (
      val hasher: Hasher,
      val label: String,
      val subtrees: List[MerkleTree]
  ) extends MerkleTree {
    override def toString: String =
      s"MerkleTree.Node($label, ${subtrees.mkString(", ")})"
    lazy val hash: Either[String, Array[Byte]] =
      subtrees
        .map(_.hash)
        .foldLeft[Either[String, Array[Byte]]](Right(Array[Byte]())) {
          (acc, el) =>
            acc.flatMap(a => el.map(b => a ++ b))
        }
        .map(hasher.hash(_))

  }

  /** Renders a given [[MerkleTree]] as a vector of strings.
    *
    * This representation is purely for visualisation – for reversible
    * serialisation use [[serialise]]
    *
    * @param tree
    * @param colors
    * @return
    */
  def render(tree: MerkleTree, colors: Boolean = true): Vector[String] = {

    def red(s: String) = if (colors) Console.RED + s + Console.RESET else s
    def blue(s: String) = if (colors) Console.BLUE + s + Console.RESET else s
    def bold(s: String) = if (colors) Console.BOLD + s + Console.RESET else s
    def yellow(s: String) =
      if (colors) Console.YELLOW + s + Console.RESET else s

    def go(
        nodes: List[MerkleTree],
        level: Int,
        result: Vector[String]
    ): Vector[String] = {
      val tab = if (level != 0) "└" + ("─" * level) + " " else ""
      val indent = " " * tab.length

      nodes match {
        case Nil                        => result
        case (l: MerkleTree.Leaf) :: tl =>
          val toBytesLabel = ToBytes
            .toStringID(l.toBytes)
            .map(l => yellow(l + " "))
            .getOrElse("")

          def trimOr(value: String, length: Int = 20) =
            if (value.length > length) value.substring(0, length) + "..."
            else value

          val hashableLabel = l.toBytes.hashableString(l.data) match {
            case Right(value) if l.toBytes != ToBytes.Str =>
              toBytesLabel + yellow(s"(${trimOr(value)}) ")
            case _ => toBytesLabel
          }

          val isError = l.hashString.isLeft
          val hashStrRaw =
            l.hashString.fold(identity, identity)

          def err(l: String) = if (isError && colors) red(l) else blue(l)

          val rd = l.toBytes.serialise(l.data)

          val lines = Seq(
            Seq(
              tab,
              bold(l.label + ": "),
              hashableLabel,
              if (l.label == rd) "" else rd
            ).mkString,
            indent + err(hashStrRaw)
          )

          go(
            tl,
            level,
            result ++ lines
          )
        case (l: MerkleTree.Node) :: tl =>
          val isError = l.hashString.isLeft
          val hashStrRaw =
            l.hashString.fold(identity, identity)

          def err(l: String) = if (isError && colors) red(l) else blue(l)

          val lines = Seq(
            tab + bold(l.label),
            indent + err(hashStrRaw)
          )
          go(l.subtrees, level + 1, result ++ lines) ++ go(
            tl,
            level,
            Vector.empty
          )

      }

    }

    go(List(tree), 0, Vector.empty)
  }

}
