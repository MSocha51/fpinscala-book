package fpinscala.exercises.state

import fpinscala.exercises.state.RNG.{Simple, doubleInt, ints}


object Test extends App:
  println(doubleInt(Simple(252251061)))


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (a, next) = rng.nextInt
    val nonNegative = if a == Int.MinValue then 0 else a.abs
    (nonNegative, next)

  def double(rng: RNG): (Double, RNG) =
    val (a, next) = nonNegativeInt(rng)
    val d = a.toDouble / (Int.MaxValue.toDouble + 1)
    (d, next)

  def _double: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, n) = rng.nextInt
    val (d, next) = double(n)
    ((i, d), next)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (p, next) = intDouble(rng)
    (p.swap, next)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, n1) = double(rng)
    val (d2, n2) = double(n1)
    val (d3, n3) = double(n2)
    ((d1, d2, d3), n3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (Nil, rng)
    else
      val (i, n) = rng.nextInt
      val (is, nextRng) = ints(count - 1)(n)
      (i :: is, nextRng)


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, na) = ra(rng)
      val (b, nb) = rb(na)
      (f(a, b), nb)
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng => rs.foldRight((List.empty[A], rng)) {
      case (r, (acc, n)) =>
        val (a, next) = r(n)
        (a :: acc, next)
    }

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, n) = r(rng)
      f(a)(n)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i =>
      val mod = i % n
      if (i + (n - 1) - mod) >= 0 then
        unit(mod)
      else
        nonNegativeLessThan(n)
    )
  }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      state => {
        val (a, ns) = run(state)
        f(a).run(ns)
      }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = state => (a, state)

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit(Nil: List[A]))((state, acc) => state.map2(acc)((a, as) => a :: as))

  def traverse[S, A, B](list: List[A])(f: A => State[S, B]): State[S, List[B]] =
    list.foldRight(unit[S, List[B]](Nil))((l, acc) => f(l).map2(acc)((b, as) => b :: as))

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for
        _ <- State.traverse(inputs)(i => State.modify(m => simulateInput(m, i)))
        finalMachine <- State.get
      yield (finalMachine.coins, finalMachine.candies)
  }

  def simulateInput(machine: Machine, input: Input): Machine = {
    if (machine.candies <= 0) {
      machine
    } else {
      input match
        case Input.Coin => simulateCoin(machine)
        case Input.Turn => simulateTurn(machine)
    }
  }

  def simulateCoin(machine: Machine): Machine = {
    if (machine.locked)
      Machine(false, machine.candies, machine.coins + 1)
    else
      machine
  }

  def simulateTurn(machine: Machine): Machine = {
    if (machine.locked)
      machine
    else
      Machine(true, machine.candies - 1, machine.coins)
  }
