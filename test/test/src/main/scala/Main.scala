abstract class Generator[+T]:
  def generate(): T

extension [T,S](g: Generator[T])
  def map(f: T=>S) = new Generator[S]:
    def generate() = f(g.generate())

  def flatMap(f: T=>Generator[S]): Generator[S] =
    new Generator[S]:
      def generate(): S =
        f(g.generate()).generate()

object Generator:
  val integers = new Generator[Int]:
    val rand = java.util.Random()
    def generate(): Int = rand.nextInt()

  val booleans = for x <- integers yield x > 0

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] =
    for x <- t; y<-u yield (x,y)


@main def hello(): Unit =
  val d = Generator.pairs(Generator.integers, Generator.integers)
  println()


