package merkle

import java.nio.file.Path
import java.security.MessageDigest
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.FileVisitOption

/** A mechanism to control how the hashable data is produced from user-provided
  * data. For example, we might use [[Path]]s in our tree construction, but when
  * hashing them we don't want to treat them as strings, but rather read the
  * file contents and hash those instead.
  */
abstract class ToBytes[+A] extends Product with Serializable {
  def hashableData(raw: Array[Byte]): Result[Array[Byte]]

  def hashableString(raw: Array[Byte]): Result[String] = {
    try {
      hashableData(raw).flatMap { data =>
        Result(new String(data, "UTF-8"))
      }
    } catch {
      case _: java.io.UnsupportedEncodingException =>
        Result.failMsg("Unsupported encoding")
    }
  }

  def serialise(raw: Array[Byte]): String
  def deserialise(raw: String): Array[Byte]

}

object ToBytes {
  private sealed trait Builtin {
    self: ToBytes[Any] =>
  }
  def fromStringID(s: String): Option[ToBytes[Any]] = s.toLowerCase match {
    case "str"          => Some(Str)
    case "filemtime"    => Some(FileMtime)
    case "filecontents" => Some(FileContents)
    case "treemtime"    => Some(TreeMtime)
    case _              => None
  }

  def toStringID(tag: ToBytes[Any]): Option[String] = tag match {
    case Str          => Some("str")
    case FileMtime    => Some("filemtime")
    case FileContents => Some("filecontents")
    case TreeMtime    => Some("treemtime")
    case _            => None
  }

  /** Instance that hashes arbitrary array of bytes. When serialised, the array
    * is written in base64 encoding
    */
  case object Arr extends ToBytes[Array[Byte]] {
    override def hashableData(raw: Array[Byte]): Result[Array[Byte]] =
      Result(raw)

    override def hashableString(raw: Array[Byte]): Result[String] =
      Result.failMsg("Raw byte arrays don't have a String encoding")

    lazy val encoder = java.util.Base64.getEncoder()
    lazy val decoder = java.util.Base64.getDecoder()

    override def serialise(raw: Array[Byte]): String =
      encoder.encodeToString(raw)

    override def deserialise(raw: String): Array[Byte] =
      decoder.decode(raw)
  }

  case object Str extends ToBytes[String] {
    override def hashableData(raw: Array[Byte]): Result[Array[Byte]] =
      Result(raw)

    override def serialise(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def deserialise(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")
  }

  /** A [[ToBytes]] instance that hashes the file modification time of a
    * [[Path]]
    */
  case object FileMtime extends ToBytes[Path] {
    override def hashableData(
        pathBytes: Array[Byte]
    ): Result[Array[Byte]] = {
      val path = Paths.get(new String(pathBytes))
      if (Files.exists(path)) {
        Result(
          Files.getLastModifiedTime(path).toMillis.toString.getBytes("UTF-8")
        )
      } else {
        Result.failMsg(s"Path $path does not exist")
      }
    }

    override def serialise(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def deserialise(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")

  }

  /** A [[ToBytes]] instance that hashes the full file contents of a [[Path]] */
  case object FileContents extends ToBytes[Path] {
    override def hashableData(
        pathBytes: Array[Byte]
    ): Result[Array[Byte]] = {
      val path = Paths.get(new String(pathBytes))
      if (Files.exists(path)) {
        val contents =
          scala.io.Source.fromFile(path.toAbsolutePath().toString).mkString
        Result(contents.getBytes("UTF-8"))
      } else {
        Result.failMsg(s"Path $path does not exist")
      }
    }

    override def serialise(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def deserialise(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")

  }

  /** A [[ToBytes]] instance that visits all the subpaths of a given [[Path]]
    * and hashes the most recent modification time of all the paths
    */
  case object TreeMtime extends ToBytes[Path] {
    override def hashableData(
        pathBytes: Array[Byte]
    ): Result[Array[Byte]] = {
      val path = Paths.get(new String(pathBytes))
      if (Files.exists(path)) {
        Result {
          Files
            .walk(path, FileVisitOption.FOLLOW_LINKS)
            .map[Long](Files.getLastModifiedTime(_).toMillis)
            .max(_.compareTo(_))
            .map[Option[Long]](Some(_))
            .orElse(Option.empty)
            .getOrElse(-1)
            .toString()
            .getBytes("UTF-8")
        }
      } else {
        Result.failMsg(s"Path $path does not exist")
      }
    }

    override def serialise(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def deserialise(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")

  }

}
