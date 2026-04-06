package merkle

import Result.*
sealed trait Result[+A] extends Product with Serializable {
  def getOrThrow(): A =
    this match {
      case Ok(value) => value
      case Fail(err) => throw err
    }

  def fold[B](ok: A => B, err: Err => B): B =
    this match {
      case Ok(value) => ok(value)
      case Fail(e)   => err(e)
    }

  def toOption: Option[A] =
    this match {
      case Ok(value) => Some(value)
      case _         => None
    }

  def isErr: Boolean =
    this match {
      case Fail(_) => true
      case _       => false
    }

  def toEither: Either[Err, A] =
    this match {
      case Ok(value) => Right(value)
      case Fail(err) => Left(err)
    }

  def flatMap[B](f: A => Result[B]): Result[B] =
    this match {
      case Ok(value) => f(value)
      case Fail(err) => Fail(err)
    }

  def map[B](f: A => B): Result[B] =
    this match {
      case Ok(value) => Ok(f(value))
      case Fail(err) => Fail(err)
    }
}

class Err(msg: String, cause: Throwable = null) extends Exception(msg, cause)
object Err {
  object TreesDiverged extends Err("Hash trees diverged")

  case class CannotParseSerialisedLine(line: String)
      extends Err(s"Cannot parse serialised line: $line")

  case class UnknownHasherID(hasherId: String)
      extends Err(s"Unknown hasher ID: $hasherId")

  case class UnknownToBytesID(id: String)
      extends Err(s"Unknown ToBytes ID: $id")

  case class NoIDForToBytes(tb: ToBytes[Any])
      extends Err(s"No ID for ToBytes: ${tb}")

  case class NoIDForHasher(hasher: Hasher)
      extends Err(s"No ID for Hasher: ${hasher}")

  object InvalidTreeFormat extends Err("Invalid tree format")

  def apply(msg: String, cause: Throwable = null): Err = new Err(msg, cause)
}

object Result {
  case class Ok[+A](value: A) extends Result[A]
  case class Fail(error: Err) extends Result[Nothing]

  def apply[A](value: A): Result[A] = Ok(value)
  def failMsg[A](error: String): Result[A] = Fail(Err(error))
  def fail[A](error: Err): Result[A] = Fail(error)

  def fromOption[A](opt: Option[A], err: => Err): Result[A] =
    opt match {
      case Some(value) => Ok(value)
      case None        => Fail(err)
    }
}
