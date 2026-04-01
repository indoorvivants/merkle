//> using scala 2.12
//> using option -Xsource:3
import java.nio.file.Path
import java.security.MessageDigest;
import java.nio.file.Paths
import java.nio.file.Files

case class NativeConfig(
    clang: Path
)

case class BuildConfig(
    compiler: NativeConfig,
    classpath: Seq[Path],
    scalafmt: Path
)

sealed abstract class ToBytes[+A] extends Product with Serializable {
  def produce(raw: Array[Byte]): Either[String, Array[Byte]]
  def label: Option[String] = None
}
object ToBytes {
  case object Str extends ToBytes[String] {
    override def produce(raw: Array[Byte]): Either[String, Array[Byte]] = Right(
      raw
    )
  }

  case object FileMtime extends ToBytes[Path] {
    override def produce(
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

    override def label: Option[String] = Some("mtime")
  }

  case object FileContents extends ToBytes[Path] {
    override def produce(
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
  }

}

sealed trait MerkleTree extends Product with Serializable {
  def label: String
  def hash: Either[String, Array[Byte]]
  def hashString: Either[String, String] = hash.map { hash =>
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
    def go(
        nodes: List[MerkleTree],
        level: Int,
        result: Vector[String]
    ): Vector[String] = {
      val tab = if (level != 0) "└" + ("─" * level) + " " else ""

      nodes match {
        case Nil                                       => result
        case (l @ MerkleTree.Leaf(label, _, tb)) :: tl =>
          val toBytesLabel = tb.label
            .map(l => Console.YELLOW + l + ": " + Console.RESET)
            .getOrElse("")

          val isError = l.hashString.isLeft
          val hashStrRaw =
            l.hashString.fold(identity, identity).grouped(64)

          val lines = hashStrRaw.toVector.zipWithIndex.map { case (e, i) =>
            val colorStart = if (isError) Console.RED else ""
            val colorEnd = if (isError) Console.RESET else ""
            if (i == 0)
              colorStart +
                e.padTo(
                  64,
                  ' '
                ) + colorEnd + " | " + tab + toBytesLabel + label
            else
              colorStart + e.padTo(64, ' ') + colorEnd + " | "
          }
          go(
            tl,
            level,
            result ++ lines
          )
        case (l @ MerkleTree.Node(label, subtrees)) :: tl =>
          val isError = l.hashString.isLeft
          val hashStrRaw =
            l.hashString.fold(identity, identity).grouped(64)

          val lines = hashStrRaw.toVector.zipWithIndex.map { case (e, i) =>
            val colorStart = if (isError) Console.RED else ""
            val colorEnd = if (isError) Console.RESET else ""

            if (i == 0)
              colorStart + e.padTo(64, ' ') + colorEnd + " | " + tab + label
            else
              colorStart + e.padTo(64, ' ') + colorEnd + " | "
          }

          go(subtrees, level + 1, result ++ lines) ++ go(
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

  case class Leaf(label: String, data: Array[Byte], toBytes: ToBytes[Any])
      extends MerkleTree {
    lazy val hash = toBytes.produce(data).map(sha)
  }
  case class Node(label: String, subtrees: List[MerkleTree])
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
}

object Main extends App {
  import MerkleTree.*

  def nest[T](label: String)(fields: T => Seq[MerkleTree]) =
    (t: T) => MerkleTree.Node(label, fields(t).toList)

  def string(label: String, s: String) = Leaf(s, s.getBytes, ToBytes.Str)

  def sortedStrings(label: String, s: Seq[String]) =
    Node(label, s.sorted.toList.map(i => string(i, i)))

  def path(label: String, p: Path, tb: ToBytes[Path]) =
    Leaf(p.toAbsolutePath().toString, p.toAbsolutePath().toString.getBytes, tb)

  def sortedPaths(label: String, s: Seq[Path], tb: ToBytes[Path]) =
    Node(
      label,
      s.sorted.toList.map(i =>
        Leaf(
          i.toAbsolutePath().toString,
          i.toAbsolutePath().toString.getBytes,
          tb
        )
      )
    )

  val builder =
    nest[BuildConfig]("BuildConfig") { bc =>
      Seq(
        nest[NativeConfig]("NativeConfig") { nc =>
          Seq(
            path("clang", nc.clang, ToBytes.FileMtime)
          )
        }(bc.compiler),
        sortedPaths(
          "classpath",
          bc.classpath,
          ToBytes.FileMtime
        ),
        path(
          "scalafmtConf",
          bc.scalafmt,
          ToBytes.FileContents
        )
      )
    }

  val tree = builder(
    BuildConfig(
      NativeConfig(Paths.get("/usr/bin/clang")),
      Seq(Paths.get("main.scala")),
      Paths.get(".scalafmt.conf")
    )
  )

  println(tree.render(true))
}
