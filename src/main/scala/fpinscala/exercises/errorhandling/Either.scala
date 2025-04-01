package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(value) =>  Left(value)
    case Right(value) => Right(f(value))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Right(v) => f(v)
      case Left(v) => Left(v)

  def map_viaFlatMap[B](f: A => B): Either[E, B] =
    flatMap(v => Right(f(v)))

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case r @ Right(v) => r
      case _ => b

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldLeft(Either.Right[E,List[B]](List()))((acc, a) => 
      acc.map2(f(a))(_.appended(_)))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa,bb))
      case (Left(e), Right(_)) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Left(ea), Left(eb)) => Left(ea ++ eb)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldLeft(Either.Right[List[E], List[B]](List()))((acc, a) =>
      map2All(acc, f(a), _.appended(_)))

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(as, x => x)
