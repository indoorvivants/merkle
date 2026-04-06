package merkle

import java.security.MessageDigest
import java.security.NoSuchAlgorithmException

/** A hasher is used to compute the hash of a [[MerkleTree]] node.
  */
trait Hasher {
  def hash(bytes: Array[Byte]): Array[Byte]
}

object Hasher {

  /** Creates a [[Hasher]] that uses a [[java.security.MessageDigest]] to
    * compute hashes.
    *
    * @param name
    *   hash name (as used in [[MessageDigest.getInstance]])
    * @return
    */
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
