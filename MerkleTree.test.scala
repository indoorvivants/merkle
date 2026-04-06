import merkle.*
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import merkle.DiffTree.DiffOutcome

class MerkleTreeTest extends munit.FunSuite {
  test("construction") {
    import Data.*

    fileSystem(
      Map(
        "clang" -> "hello",
        "main.scala" -> "world",
        ".scalafmt.conf" -> "runner.dialect = 3"
      )
    ) { fs =>
      val config = BuildConfig(
        "hello",
        NativeConfig(fs.path("clang")),
        Seq(fs.path("main.scala")),
        fs.path(".scalafmt.conf")
      )

      val hashBefore = {
        val tree = buildConfigHasher(config)
        tree.hashString
      }

      fs.write(".scalafmt.conf", "runner.dialect = 213")

      val hashAfter = {
        val tree = buildConfigHasher(config)
        tree.hashString
      }

      assertNotEquals(hashAfter, hashBefore)

      // hash is stable
      assertEquals(hashAfter, buildConfigHasher(config).hashString)
    }
  }

  test("serialisation") {
    import Data.*
    fileSystem(
      Map(
        "clang" -> "hello",
        "main.scala" -> "world",
        ".scalafmt.conf" -> "runner.dialect = 3"
      )
    ) { fs =>
      val config = BuildConfig(
        "hello",
        NativeConfig(fs.path("clang")),
        Seq(fs.path("main.scala")),
        fs.path(".scalafmt.conf")
      )

      val tree = buildConfigHasher(config)

      val serialised = MerkleTree.serialise(tree).right.get

      val readBack = MerkleTree.deserialise(serialised).right.get

      val hashFromRead = readBack.hashString.right.get

      // Without changes, the serialised version should have the same hash
      assertEquals(tree.hashString.right.get, hashFromRead)

      fs.write(".scalafmt.conf", "runner.dialect = 213")

      assertNotEquals(
        buildConfigHasher(config).hashString.right.get,
        hashFromRead
      )

    }

  }

  test("diffing") {
    import Data.*
    fileSystem(
      Map(
        "clang" -> "hello",
        "main.scala" -> "world",
        ".scalafmt.conf" -> "runner.dialect = 3"
      )
    ) { fs =>
      val config = BuildConfig(
        "hello",
        NativeConfig(fs.path("clang")),
        Seq(fs.path("main.scala")),
        fs.path(".scalafmt.conf")
      )

      val tree = buildConfigHasher(config)

      val tree2 = buildConfigHasher(config.copy(artifactName = "bye"))

      val empty = diffTree(tree, tree)

      assertEquals(empty, DiffTree.DNode("BuildConfig", DiffOutcome.Same, Nil))

      def stringHash(name: String) =
        builder.string("artifactName", name).hashString.right.get

      val diff = diffTree(tree, tree2)
      assertEquals(
        diff,
        DiffTree.DNode(
          "BuildConfig",
          DiffOutcome
            .Modified(tree.hashString.right.get, tree2.hashString.right.get),
          List(
            DiffTree.DLeaf(
              "artifactName",
              DiffOutcome.Modified(
                stringHash("hello"),
                stringHash("bye")
              )
            )
          )
        )
      )
    }
  }

}

object Data {
  class FileSystem private[Data] (
      mapping: Map[String, String],
      location: Path
  ) {
    def path(n: String) = {
      assert(
        mapping.contains(n),
        s"path $n not found in file system. Known paths: ${mapping.keySet.mkString(", ")}"
      )
      location.resolve(n)
    }

    // def touch(n: String) = {
    //   Files.write(path(n), Files.readString(path(n)).getBytes())
    // }

    // We add a sleep here because sometimes execution is so fast
    def sleepAndTouch(n: String, duration: Long = 500L) = {
      Thread.sleep(duration)
      Files.write(path(n), Files.readString(path(n)).getBytes())
    }

    def write(n: String, contents: String) = {
      Files.write(path(n), contents.getBytes())
    }
  }

  def fileSystem(mapping: Map[String, String])(f: FileSystem => Unit) = {
    var tmp = Option.empty[Path]
    try {
      val loc = Files.createTempDirectory("merkle-test")
      tmp = Some(loc)
      val fs = new FileSystem(mapping, loc)
      mapping.foreach { case (n, contents) => fs.write(n, contents) }
      f(fs)
    } finally {
      tmp.foreach { loc =>
        val files = List.newBuilder[Path]
        Files.walk(loc).forEach { p =>
          files += p
        }

        files
          .result()
          .sortBy(p => (Files.isDirectory(p), -p.toString().length))
          .foreach(Files.deleteIfExists(_))
        Files.deleteIfExists(loc)
      }
    }
  }

  def diffTree(tree: MerkleTree, tree2: MerkleTree) = {
    DiffTree.create(tree, tree2).right.get
  }

  case class NativeConfig(
      clang: Path
  )

  case class BuildConfig(
      artifactName: String,
      compiler: NativeConfig,
      classpath: Seq[Path],
      scalafmt: Path
  )

  import MerkleTree.*

  val builder = new Builder(Hasher.messageDigest("sha-256"))

  import builder.*

  val nativeConfigHasher =
    nest[NativeConfig]("NativeConfig") { nc =>
      Seq(
        path("clang", nc.clang, ToBytes.FileMtime)
      )
    }

  val buildConfigHasher =
    nest[BuildConfig]("BuildConfig") { bc =>
      Seq(
        string("artifactName", bc.artifactName),
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

}
