package merkle

import java.nio.file.Path
import java.security.MessageDigest
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.FileVisitOption

abstract class ToBytes[+A] extends Product with Serializable {
  def hashableData(raw: Array[Byte]): Either[String, Array[Byte]]

  def hashableString(raw: Array[Byte]): Either[String, String] = {
    try {
      hashableData(raw).flatMap { data =>
        Right(new String(data, "UTF-8"))
      }
    } catch {
      case _: java.io.UnsupportedEncodingException =>
        Left("Unsupported encoding")
    }
  }

  def label: Option[String] = None

  def renderData(raw: Array[Byte]): String
  def serialiseData(raw: String): Array[Byte]

}

object ToBytes {
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

  case object Str extends ToBytes[String] {
    override def hashableData(raw: Array[Byte]): Either[String, Array[Byte]] =
      Right(
        raw
      )

    override def renderData(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def serialiseData(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")
  }

  case object FileMtime extends ToBytes[Path] {
    override def hashableData(
        pathBytes: Array[Byte]
    ): Either[String, Array[Byte]] = {
      val path = Paths.get(new String(pathBytes))
      if (Files.exists(path)) {
        Right(
          Files.getLastModifiedTime(path).toMillis.toString.getBytes("UTF-8")
        )
      } else {
        Left(s"Path $path does not exist")
      }
    }

    override def renderData(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def serialiseData(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")

    override def label: Option[String] = Some("mtime")
  }

  case object FileContents extends ToBytes[Path] {
    override def hashableData(
        pathBytes: Array[Byte]
    ): Either[String, Array[Byte]] = {
      val path = Paths.get(new String(pathBytes))
      if (Files.exists(path)) {
        val contents =
          scala.io.Source.fromFile(path.toAbsolutePath().toString).mkString
        Right(contents.getBytes("UTF-8"))
      } else {
        Left(s"Path $path does not exist")
      }
    }

    override def label: Option[String] = Some("content")

    override def renderData(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def serialiseData(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")

  }

  case object TreeMtime extends ToBytes[Path] {
    override def hashableData(
        pathBytes: Array[Byte]
    ): Either[String, Array[Byte]] = {
      val path = Paths.get(new String(pathBytes))
      if (Files.exists(path)) {
        Right {
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
        Left(s"Path $path does not exist")
      }
    }

    override def renderData(raw: Array[Byte]): String =
      new String(raw, "UTF-8")

    override def serialiseData(raw: String): Array[Byte] =
      raw.getBytes("UTF-8")

    override def label: Option[String] = Some("mtime")
  }

}
