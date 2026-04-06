package merkle

import java.nio.file.Path
import java.security.MessageDigest
import java.security.NoSuchAlgorithmException
import scala.annotation.nowarn

trait Hasher {
  def hash(bytes: Array[Byte]): Array[Byte]
}

object Hasher {
  def messageDigest(name: String): Hasher = new MessageDigestHasher(
    MessageDigest.getInstance(name)
  )

  class MessageDigestHasher(private[Hasher] val digest: MessageDigest)
      extends Hasher {
    def hash(bytes: Array[Byte]): Array[Byte] = {
      try { digest.digest(bytes) }
      finally { digest.reset() }
    }
  }

  def toStringID(h: Hasher): Option[String] = {
    h match {
      case m: MessageDigestHasher => Some(m.digest.getAlgorithm)
      case _                      => None
    }
  }

  def fromStringID(s: String): Option[Hasher] = {
    try {
      Some(messageDigest(s))
    } catch {
      case ex: NoSuchAlgorithmException => None
    }
  }
}

sealed abstract class MerkleTree {
  def label: String

  /** @return
    */
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
          val toBytesLabel = l.toBytes.label
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

          val rd = l.toBytes.renderData(l.data)

          val lines = Seq(
            Seq(
              tab,
              bold(l.label + ": "),
              hashableLabel,
              if (label == rd) "" else rd
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

    go(List(this), 0, Vector.empty).mkString("\n")
  }
}

object MerkleTree {
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
      fromBytesId: String => Option[ToBytes[Any]] = s =>
        ToBytes.fromStringID(
          s
        ), // .getOrElse(sys.error(s"Unknown ToBytes id: $s")),
      fromHasherId: String => Option[Hasher] = s =>
        Hasher.fromStringID(
          s
        ) // .getOrElse(sys.error(s"Unknown Hasher id: $s"))
  ): Either[String, MerkleTree] = {
    val (hasherID, rest) = (lines.head, lines.tail)
    val detectedHasher = fromHasherId(hasherID).getOrElse {
      return Left(s"Unknown Hasher id: $hasherID")
    }

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
              // We override the hashes in the nodes read from the file to make sure
              // they are not computed on real inputs – which is not an issue for strings, but
              // things like FileMtime will be incorrect
              node = new Node(detectedHasher, label, children) {
                @nowarn
                override lazy val hash =
                  Right[String, Array[Byte]](
                    hexToBytes(serialisedHash)
                  )

                @nowarn
                override lazy val hashString =
                  Right[String, String](serialisedHash)
              }
              rest <- go(
                t.tail.drop(childrenLines.length),
                level
              )
            } yield node :: rest
          } else if (parts.length == 5) { // leaf
            val label = parts(0).stripPrefix(indent)
            val toBytes = fromBytesId(parts(2)).getOrElse {
              return Left(s"Unknown ToBytes id: ${parts(2)}")
            }

            val stringData = parts(3)
            val serialisedData = toBytes.serialiseData(stringData)
            val serialisedHash = parts(4)

            go(t.tail, level).map { l =>
              new Leaf(detectedHasher, label, serialisedData, toBytes) {
                @nowarn
                override lazy val hash =
                  Right[String, Array[Byte]](hexToBytes(serialisedHash))

                @nowarn
                override lazy val hashString =
                  Right[String, String](serialisedHash)
              } :: l
            }
          } else { // invalid
            Left(s"cannot parse line: $line")
          }
        case None => Right(Nil)
      }
    }

    go(rest, 0).flatMap {
      case Nil         => Right(new Node(detectedHasher, "<empty tree>", Nil))
      case head :: Nil => Right(head)
      case _           => Left("Invalid tree format")
    }
  }

  class Leaf private[MerkleTree] (
      hasher: Hasher,
      val label: String,
      val data: Array[Byte],
      val toBytes: ToBytes[Any]
  ) extends MerkleTree {
    lazy val hash = toBytes.hashableData(data).map(hasher.hash(_))

    override def toString: String =
      s"MerkleTree.Leaf($label, ${toBytes.label.getOrElse("")} ${toBytes.renderData(data)})"

  }
  class Node private[MerkleTree] (
      hasher: Hasher,
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

  class Builder(hasher: Hasher) {

    def nest[T](label: String)(fields: T => Seq[MerkleTree]) =
      (t: T) => new MerkleTree.Node(hasher, label, fields(t).toList)

    def string(label: String, s: String) =
      new Leaf(hasher, label, s.getBytes, ToBytes.Str)

    def strings(label: String, s: Seq[String]) =
      new Node(hasher, label, s.toList.map(i => string(i, i)))

    def sortedStrings(label: String, s: Seq[String]) =
      new Node(hasher, label, s.sorted.toList.map(i => string(i, i)))

    def path(label: String, p: Path, tb: ToBytes[Path]) =
      new Leaf(hasher, label, p.toAbsolutePath().toString.getBytes, tb)

    def bool(label: String, value: Boolean) =
      string(label, value.toString)

    def sortedPaths(label: String, s: Seq[Path], tb: ToBytes[Path]) =
      new Node(
        hasher,
        label,
        s.sorted.toList.map(i =>
          new Leaf(
            hasher,
            i.toAbsolutePath().toString,
            i.toAbsolutePath().toString.getBytes,
            tb
          )
        )
      )
  }
}
