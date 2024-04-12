package fpinscala.exercises.state


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
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    i -> d-> r3

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d2, r4) = double(r3)
    ((d1, d2, d3), r4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then
      (List(), rng)
    else
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, r1) ra(rng)
      val (b, r2) rb(r1)
      (f(a, b), r2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((a, acc) => map2(r, acc)(_ :: _))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng1) = r(rng0)
      f(a)(rng1)

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
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
