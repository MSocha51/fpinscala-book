package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{empty, unfold}

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case LazyList.Empty => Nil
    case LazyList.Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    if n == 0 then empty
    else this match
      case LazyList.Empty => empty
      case LazyList.Cons(h, t) => LazyList.cons(h(), t().take(n-1))

  def drop(n: Int): LazyList[A] =
    if n == 0 then this
    else this match
    case LazyList.Empty => empty
    case LazyList.Cons(h, t) => t().drop(n - 1)

  def takeWhile2(p: A => Boolean): LazyList[A] =
    this match
    case LazyList.Cons(h, t) if p(h())=> LazyList.cons(h(), t().takeWhile(p))
    case _ => empty

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, acc) =>
      if p(a) then LazyList.cons(a, acc) else empty)


  def headOption: Option[A] =
    foldRight(Option.empty[A])((head, acc) => Option(head))

  def tail: LazyList[A] =
    this match
      case LazyList.Empty => empty
      case LazyList.Cons(h, t) => t()

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, acc) => LazyList.cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if p(a) then LazyList.cons(a, acc) else acc)

  def append[AA >: A](that: => LazyList[AA]): LazyList[AA] =
    foldRight(that)((a, acc) => LazyList.cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this)(xs => xs.headOption.map(a => (f(a), xs.tail)))

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((n, this))((nn, xs) =>
      if nn > 0 then xs.headOption.map(a => (a, (nn - 1, xs.tail))) else None)

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this)(xs => xs.headOption.filter(p).map(a => (a, xs.tail)))

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that))((xs, ys) =>
      xs.headOption.flatMap(x => ys.headOption.map(y => (f(x,y), (xs.tail, ys.tail)))))

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that))((xs, ys) =>
      (xs.headOption, ys.headOption) match
        case (None, None) => None
        case p @ _  => Option((p, (xs.tail, ys.tail)))
    )

  def startsWith[AA >: A](s: LazyList[AA]): Boolean =
    this == s || this.zipAll(s).foldRight(true){ (p, acc) =>
      acc && (p match
        case (None, Some(_)) => false
        case (Some(_), None) => true
        case (a, b) => a == b
        )
    }

  def tails: LazyList[LazyList[A]] =
    unfold(this)(xs =>
      if xs != empty then Option((xs, xs.tail)) else None
    ).append(LazyList(empty))

  def hasSubsequence[AA >: A](l: LazyList[AA]): Boolean = {
    tails.exists(_.startsWith(l))
  }
  
  def scanRight[B](init: B)(f: (A,B) => B): LazyList[B] =
    foldRight(LazyList(init))((a, bx) =>
      bx match
        case Cons(b, _) => LazyList.cons(f(a, b()), bx) 
    )

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
        LazyList.cons(current, go(next, next + current))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((current, next)
  => Option((current), (next, current + next)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(a => Some(a, a + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(a => Some(a, a))

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(a => Some((a, a)))
