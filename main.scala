//> using scala 2.12
//> using option -Xsource:3
import java.nio.file.Path
import java.security.MessageDigest
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.FileVisitOption
import DiffTree.DNode
import DiffTree.DLeaf
import DiffTree.Action.Added
import DiffTree.Action.Modified
import DiffTree.Action.Removed
import DiffTree.Action.Same

case class NativeConfig(
    clang: Path
)

case class BuildConfig(
    compiler: NativeConfig,
    classpath: Seq[Path],
    scalafmt: Path
)

object Main extends App {
  import MerkleTree.*

  val nativeConfigHasher =
    nest[NativeConfig]("NativeConfig") { nc =>
      Seq(
        path("clang", nc.clang, ToBytes.FileMtime)
      )
    }

  val buildConfigHasher =
    nest[BuildConfig]("BuildConfig") { bc =>
      Seq(
        nativeConfigHasher(bc.compiler),
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

  val tree = buildConfigHasher(
    BuildConfig(
      NativeConfig(Paths.get("/usr/bin/clang")),
      Seq(Paths.get("main.scala")),
      Paths.get(".scalafmt.conf")
    )
  )

  val tree2 = buildConfigHasher(
    BuildConfig(
      NativeConfig(Paths.get("/usr/bin/clang++")),
      Seq(Paths.get(".mcp.json")),
      Paths.get(".gitignore")
    )
  )

  println(tree.render(true))
  println("---")
  println(tree2.render(true))

  println("---")
  println(DiffTree.create(tree, tree2).right.get.render())

  println("---")

  println(MerkleTree.serialise(tree).mkString("\n"))

  println(MerkleTree.read(MerkleTree.serialise(tree)).right.get.render())
}
