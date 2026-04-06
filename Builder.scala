package merkle

import MerkleTree.{Leaf, Node}
import java.nio.file.Path

/** Helpers for construction of MerkleTree instances
  *
  * @param hasher
  */
class Builder(hasher: Hasher) {

  /** Creates a new node in the tree from a function that takes some value as
    * input and produces a sequence of subtrees.
    *
    * This is the primary function for implementing a case class to MerkleTree
    * mapping
    *
    * @param label
    * @param fields
    * @return
    */
  def nest[T](label: String)(fields: T => Seq[MerkleTree]) =
    (t: T) => new Node(hasher, label, fields(t).toList)

  def string(label: String, s: String): MerkleTree =
    new Leaf(hasher, label, s.getBytes, ToBytes.Str)

  /** A node that contains given sequence as strings as sequence of individually
    * hashed leaf nodes. Strings are added to the tree in the order they are
    * given, without sorting
    *
    * @param label
    * @param s
    * @return
    */
  def strings(label: String, s: Seq[String]): MerkleTree =
    new Node(hasher, label, s.toList.map(i => string(i, i)))

  /** A node that contains given sequence as strings as sequence of individually
    * hashed leaf nodes. The strings are sorted before being added to the tree
    *
    * @param label
    * @param s
    * @return
    */
  def sortedStrings(label: String, s: Seq[String]): MerkleTree =
    new Node(hasher, label, s.sorted.toList.map(i => string(i, i)))

  /** Store a path as a leaf node using a given [[ToBytes]] instance. The
    * absolute path is converted to a String and used as input to [[ToBytes]]
    */
  def path(label: String, p: Path, tb: ToBytes[Path]): MerkleTree =
    new Leaf(hasher, label, p.toAbsolutePath().toString.getBytes, tb)

  /** Store a bool value as a leaf node. The value is converted to [[String]]
    * and then serialised with [[string]]
    */
  def bool(label: String, value: Boolean): MerkleTree =
    string(label, value.toString)

  /** A node that contains given sequence of paths as sequence of individually
    * hashed leaf nodes using a given [[ToBytes]] instance. The paths are sorted
    * before being added to the tree
    *
    * @param label
    * @param s
    * @param tb
    * @return
    */
  def sortedPaths(label: String, s: Seq[Path], tb: ToBytes[Path]): MerkleTree =
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

  def leaf(label: String, data: Array[Byte], tb: ToBytes[Any]): MerkleTree =
    new Leaf(hasher, label, data, tb)

  def node(label: String, subtrees: Seq[MerkleTree]): MerkleTree =
    new Node(hasher, label, subtrees.toList)
}
