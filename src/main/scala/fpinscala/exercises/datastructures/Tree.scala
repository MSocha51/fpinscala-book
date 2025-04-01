package fpinscala.exercises.datastructures

import fpinscala.exercises.datastructures.Tree.Branch

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(x) => f(x)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int = fold(_ => 1, _ + _ + 1)

  def depthViaFold: Int = fold(_ => 0, 1 + _.max(_))

  def mapViaFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(left, right) => left.maximum.max(right.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
