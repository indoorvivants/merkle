# merkle – Scala 3 implementation of Merkle trees and their diffs

This project aims to provide:

1. A customisable Merkle tree builder
2. Serialisation and deserialisation of Merkle Trees
3. Calculation of differences between two given merkle trees of similar structure
4. Pretty rendering

All of that published for Scala 2.12, 2.13, 3.x, JVM and Native.

# Installation

- **SBT**: `libraryDependencies += "com.indoorvivants" %%% "merkle" % "<latest version>"`
- **Scala CLI**: `//> using dep com.indoorvivants::merkle::<latest version>`
- **Mill**: `ivy"com.indoorvivants::merkle::<latest version>"`

# Introduction and quick start

Let's say we have the following case classes that represents the state of our application and `build` function that takes the state and produces an artifact (which is a slow process):

```scala
import java.nio.file.Path 

case class NativeConfig(
  optimize: Boolean,
  debug: Boolean,
  staticLib: Path
)

case class BuildConfig(
  artifactName: String,
  // path to `clang` compiler binary
  compilerPath: Path,
  // path to a json file containing profiling info used during build time
  profileInfo: Path,
  native: NativeConfig,
  output: Path
)

def build(config: BuildConfig): Unit = ???
```

Here are the requirements:

1. When `build` function is called, we only want to run the slow build logic if `config` has changed from the previous run. Doing this in memory is simple, but we want to persist this information between process restarts

2. Additionally, when the config does change, we want detailed information on what exactly changed

While (1) can be solved (in simple cases, without `Path`s) by simple saving the  `hashCode` of `BuildConfig`, requirement (2) is harder to satisfy.

## Tree construction

To support our usecases, let's mirror our build config using the Merkle tree builder:

```scala
import merkle.*

val builder = new Builder(Hasher.messageDigest("SHA-256"))
import builder.*
```

This will produce some helper methods and set the hashing algorithm to be SHA-256 (backed by `MessageDigest`).

Our fields need to be serialised differently depending on the type

1. `String` and `Boolean` can have their contents hashed directly

2. For `Path` in `compilerPath` the only thing that matters is the last modified time of the path – e.g. if it's `/usr/bin/clang` we only care when that file was changed, when a new version of clang is installed
   - Same for `Path` in `NativeConfig#staticLib`

3. For `Path` in `profileInfo` the only thing that matters is the contents of the file – the profiler can run many times, but if it produces the exact same contents, we shouldn't re-run the build

Let's construct the tree using the methods imported from `builder`:

```scala 
val nativeConfigHasher = nest[NativeConfig]("NativeConfig") { nc => 
  Seq(
    bool("optimize", nc.optimize),
    bool("debug", nc.debug),
    path("staticLib", nc.staticLib, ToBytes.FileMtime),
  )
}

val buildConfigHasher = nest[BuildConfig]("BuildConfig") { bc => 
  Seq(
    string("artifactName", bc.artifactName),
    path("compilerPath", bc.compilerPath, ToBytes.FileMtime),
    path("profileInfo", bc.profileInfo, ToBytes.FileContents),
    path("output", bc.output, ToBytes.FileMtime),
    nativeConfigHasher(bc.native) 
  )
}
```

We do this construction by hand to have control over what parts of the
config should be hashable (if there's a part that changes with every build, we want to ignore that). In the future, the library will provide a way to derive a tree builder directly from case classes – the only reason it's not built in is because there needs to be a decent mechanism to cover the ambiguity in how a single type (e.g. `Path`) should be hashed.

Note that we are also hasing `output` parameter in `BuildConfig` – if the built artifact was moved or tampered with in any other way, the build should be re-triggered, because we can't guarantee that the file at `output` path is the same as what was produced by the build.

With this construction in mind, we can build the tree and pretty print it just to get a feel for what the system understands and considers. Let's create a dummy environment with real files:

```scala
import java.nio.file.Files 

// Set up the file system in a temporary folder
val tempdir = Files.createTempDirectory("hello")
Map(
  "profile.json" -> "{}",
  "clang" -> "hello I'm clang",
  "static.a" -> "hello I'm static lib",
  "bin-debug" -> "world"
).foreach { case (name, contents) => 
  Files.write(tempdir.resolve(name), contents.getBytes())
}

val bc = BuildConfig(
  artifactName = "bin",
  compilerPath = tempdir.resolve("clang"),
  profileInfo = tempdir.resolve("profile.json"),
  native = NativeConfig(
    optimize = true,
    debug = true,
    staticLib = tempdir.resolve("static.a")
  ),
  output = tempdir.resolve("bin-debug")
)

val tree = buildConfigHasher(bc)

println(tree.renderToString())
```

This will print something like

```
BuildConfig
110cc92c7b888e75fa2e0652b904c816dc031b11d1d285b28341ae7213cc14c5
└─ artifactName: str bin
   51a1f05af85e342e3c849b47d387086476282d5f50dc240c19216d6edfb1eb5a
└─ compilerPath: filemtime (1775506392799) /var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello15037354629585893581/clang
   fda620ba2837dd98f0ac486775a1228f17f26ab28abba3c1531df7fc512a25b9
└─ profileInfo: filecontents ({}) /var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello15037354629585893581/profile.json
   44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a
└─ output: filemtime (1775506392799) /var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello15037354629585893581/bin-debug
   fda620ba2837dd98f0ac486775a1228f17f26ab28abba3c1531df7fc512a25b9
└─ NativeConfig
   19336e4c9b25492db7c4f411f5424e3bb84266299d531f18f211d6db805b9b92
└── optimize: str true
    b5bea41b6c623f7c09f1bf24dcae58ebab3c0cdd90ad966bc43a45b44867e12b
└── debug: str true
    b5bea41b6c623f7c09f1bf24dcae58ebab3c0cdd90ad966bc43a45b44867e12b
└── staticLib: filemtime (1775506392799) /var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello15037354629585893581/static.a
    fda620ba2837dd98f0ac486775a1228f17f26ab28abba3c1531df7fc512a25b9
```

The rendered output shows the raw hashes, the tree structure, the 
way the data is interepreted, and the raw value used by hashing.

This pretty printing is useful when debugging or building the tree,
but reading the tree back from it is difficult – and that will be important if we want to save the state to a file. This is where serialisation comes in:

```scala
val serialised = tree.serialise().getOrThrow()

println(serialised.mkString("\n"))
```

The serialised tree format is noticeably less prety, but it's reversible:

```
SHA-256
BuildConfig	n	9327899acad88979a4f987fb78e6874d57341167602e29bfbade1d13ec8aa33c
 artifactName	l	str	bin	51a1f05af85e342e3c849b47d387086476282d5f50dc240c19216d6edfb1eb5a
 compilerPath	l	filemtime	/var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello3853547914936142397/clang	eb8a2b50a070db1b876b46cb2a46b606d3d2bab55f5ec4fbcded5d8236e2bd51
 profileInfo	l	filecontents	/var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello3853547914936142397/profile.json	44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a
 output	l	filemtime	/var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello3853547914936142397/bin-debug	7ce26c6a56c2a372400b35f4e2aef5f19674f39d584bda102c0c862daf50ae48
 NativeConfig	n	6afea0d7293de654d3d62e9aafcda6b66d56f232bcd21d45b2d21ec9a1474700
  optimize	l	str	true	b5bea41b6c623f7c09f1bf24dcae58ebab3c0cdd90ad966bc43a45b44867e12b
  debug	l	str	true	b5bea41b6c623f7c09f1bf24dcae58ebab3c0cdd90ad966bc43a45b44867e12b
  staticLib	l	filemtime	/var/folders/jr/4vy422xj6zs44hsn5d1_b4g40000gn/T/hello3853547914936142397/static.a	7ce26c6a56c2a372400b35f4e2aef5f19674f39d584bda102c0c862daf50ae48
```

This representation can be saved to a file and used as the source of truth for subsequent runs of the cached build function we're about to build.

Now that we have the tree persisted, we can modify the environment and use the deserialisation mechanism along with diffing to see if the 
build should be replayed:

```scala
val deserialised = MerkleTree.deserialise(serialised).getOrThrow()

val diff = DiffTree.create(before = tree, after = deserialised).getOrThrow()

println(diff.hasDifferences)
println(diff.renderToString())
```

This will output

```
false
= BuildConfig
```

because the tree hasn't changed – when we deserialised it from a string representation, all the hashes are set to the values recorded
in the file – equal to those in `tree`. The hashes won't be evaluated again – deserialisation enforces that.

Now let's see what happens if we modify the config and compare things again:

```scala
// Let's 
Files.write(tempdir.resolve("static.a"), "another world".getBytes)
Files.write(tempdir.resolve("clang"), "another clang".getBytes)
Files.write(tempdir.resolve("profile.json"), "{}".getBytes)

val newBuildConfig = bc.copy(artifactName = "myapp")

val newTree = buildConfigHasher(newBuildConfig)

val newDiff = DiffTree.create(before = deserialised, after = newTree).getOrThrow()

println(newDiff.hasDifferences)
println(newDiff.renderToString())
```

this will output

```
true
~ BuildConfig
└─ ~ artifactName
└─ ~ compilerPath
└─ ~ NativeConfig
└── ~ staticLib
```

The diff tree is lazily constructed to only show the differences, at 
appropriate nesting levels. Importantly, `profileInfo` key is absent – while the file was modified, the contents remained the same.

The rendered view of the diff tree can be showed to the user, and `hasDifferences` method can be used in the logic that decides whether 
the build needs to be re-run.

We now have everything to write a `buildCached` method that improves on our `build` method:

```scala
// workspace is some safe path where our tooling can create persisted 
// caches
def buildCached(config: BuildConfig, workspace: Path): Unit = {
  val cachedConfigPath = workspace.resolve("build-config")
  
  if(!Files.exists(cachedConfigPath)) {
    // possibly first run – build the artifact, record the config
    build(config)
    val hashed = buildConfigHasher(config).serialise().getOrThrow()
    Files.write(cachedConfigPath, hashed.mkString("\n").getBytes)
  } else {
    // incremental run – read the config and check against the 
    // one given to this function
    val before = MerkleTree.deserialise(Files.readString(cachedConfigPath).split("\n").toVector).getOrThrow()
    val after = buildConfigHasher(config)
    
    val diff = DiffTree.create(before, after).getOrThrow()
    
    if(diff.hasDifferences) {
      println("Build config has changed!")
      diff.render().foreach(println)
      
      build(config)
      
      // Because `output` is part of the BuildConfig,
      // we want to construct a new hash tree AFTER the build
      // to record the accurate modification time of the output path
      val forcedAfter = buildConfigHasher(config)
      
      Files.write(cachedConfigPath, forcedAfter.serialise().getOrThrow().mkString("\n").getBytes)
    } else {
      println("Build config has not changed")
    }
  }
}
```

And there we go – we have an incremental build pipeline with 
good user feedback.
