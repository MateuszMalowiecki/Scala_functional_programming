import scala.annotation.tailrec
import scala.collection.TraversableOnce.MonadOps

object Main extends App{
   def last[A](xs: List[A]) : A = {
     xs match {
       case Nil => throw new NoSuchElementException()
       case h :: Nil => h
       case _ :: t => last(t)
     }
   }
  println(last[Int](List(1,1,2,2,3,5,8)))

  def penultimate[A](xs: List[A]) : A = {
    xs match {
      case h :: _ :: Nil => h
      case _ :: t => penultimate(t)
      case _ =>  throw new NoSuchElementException()
    }
  }
  println(penultimate[Int](List(1,1,2,2,3,5,8)))

  def nth[A](n:Int, xs: List[A]) : A = {
    (n, xs) match {
      case (0, h :: t) => h
      case (n, Nil) => throw new NoSuchElementException()
      case (n, h :: t) => nth(n-1, t)
    }
  }
  println(nth[Int](3, List(1,1,2,2,3,5,8)))

  def length[A](xs: List[A]) : Int = {
    xs match {
      case Nil => 0
      case _ :: t => length(t) + 1
    }
  }
  println(length[Int](List(1,1,2,2,3,5,8)))

  def reverse[A](xs: List[A]) : List[A] = {
    @tailrec
    def tailRecReverse[A](xs : List[A], acc : List[A]):List[A] = {
      xs match {
        case Nil => acc
        case h :: t => tailRecReverse(t, h :: acc)
      }
    }
    tailRecReverse(xs, Nil)
  }
  println(reverse[Int](List(1,1,2,2,3,5,8)))

  def palindrome[A](xs: List[A]) = xs == reverse(xs)
  println(palindrome[Int](List(1,1,2,2,3,5,8)))

  def flatten[A](xs: List[List[A]]) : List[A] = {
    xs match {
        case Nil => Nil
        case h :: t => h ++ flatten(t)
    }
  }
  println(flatten[Int](List(List(1, 1), List(2, 3, 5, 8))))

  def compress[A](xs: List[A]) : List[A] = {
    xs match {
      case Nil => Nil
      case h :: Nil => h :: Nil
      case h :: h2 :: t =>
        if (h == h2) compress(h2 :: t)
        else h :: compress(h2 :: t)
    }
  }
  println(compress[Int](List(1,1,2,2,3,5,8)))

  def pack[A](xs: List[A]) : List[List[A]] = {
    xs match {
      case Nil => Nil
      case h :: t =>
        val (heads, rest) = t.span(x =>  x == h)
        (h :: heads) :: pack(rest)
    }
  }
  println(pack[Symbol](List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  def encode[A](xs: List[A]) : List[(Int, A)] = {
    pack(xs).map(xs => (length(xs), xs.head))
  }
  val enc=encode[Symbol](List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  println(enc)

  def encodeModified[A](xs: List[A]) : List[Any] = {
    pack(xs).map(xs => if (length(xs) > 1) { (length(xs), xs.head) } else {xs.head})
  }
  println(encodeModified[Symbol](List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  def decode[A](xs: List[(Int, A)]) : List[A] = {
     flatten(xs.map{ case (num, elem) => List.fill(num)(elem) })
  }
  println(decode(enc))

  def encodeDirect[A](xs: List[A]) : List[(Int, A)] = {
    xs match {
      case Nil => Nil
      case h :: t =>
        val (heads, rest) = t.span(x =>  x == h)
        (length(heads) + 1, h) :: encodeDirect(rest)
    }
  }
  val encdir=encodeDirect[Symbol](List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  println(enc == encdir)

  def duplicate[A](xs: List[A]) : List[A] = {
    xs match {
      case Nil => Nil
      case h :: t => h :: h :: duplicate(t)
    }
  }
  println(duplicate(List('a, 'b, 'c, 'c, 'd)))

  def duplicateN[A](n:Int, xs: List[A]) : List[A] = {
    xs match {
      case Nil => Nil
      case h :: t => List.fill(n)(h) ++ duplicateN(n, t)
    }
  }
  println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))

  def drop[A](n:Int, xs : List[A]):List[A] = {
    def dropWithIndex(i:Int, xs :List[A]): List[A] = {
      xs match {
        case Nil => Nil
        case h::t =>
          if (i % n == 0) dropWithIndex(i+1, t)
          else h :: dropWithIndex(i+1, t)
      }
    }
    dropWithIndex(1, xs)
  }
  println(drop[Symbol](3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  def split[A](n: Int, xs:List[A]):(List[A], List[A]) = {
    (n , xs) match {
      case (_, Nil) => (Nil ,Nil)
      case (0, _) => (Nil, xs)
      case (n, h ::t) =>
        val (before, after) = split(n - 1, t)
        (h :: before, after)
    }
  }
  println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  def slice[A](start: Int, end:Int, xs:List[A]) : List[A] = {
    val (_, third_and_bigger) = split(start, xs)
    val (between_three_and_seven, _) = split(end - start, third_and_bigger)
    between_three_and_seven
  }
  println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  def rotate[A](n:Int, xs:List[A]) : List[A] = {
    val (begin, end)=split(n ,xs)
    end ++ begin
  }
  println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  def removeAt[A](n:Int, xs:List[A]) : (List[A], A) = {
    (n, xs) match {
      case (_, Nil) => throw new NoSuchElementException()
      case (0, h::t) => (t, h)
      case (n, h::t) =>
        val (list, elem) = removeAt(n-1, t)
        (h::list, elem)
    }
  }
  println(removeAt(1, List('a, 'b, 'c, 'd)))

  def insertAt[A](elem:A, n:Int, xs:List[A]) : List[A] = {
    (n, xs) match {
      case (_, Nil) => elem :: Nil
      case (0, xs) => elem :: xs
      case (n, h::t) => h::insertAt(elem, n-1, t)
    }
  }
  println(insertAt('new, 1, List('a, 'b, 'c, 'd)))

  def range(start: Int, end: Int) : List[Int] = {
    if (start > end) Nil
    else start :: range(start + 1, end)
  }
  println(range(4, 9))

  def randomSelect[A](n:Int, xs:List[A]):List[A] = {
    if (n==0) Nil
    else {
      val r=new util.Random()
      val (list, elem) = removeAt(r.nextInt(length(xs)), xs)
      elem :: randomSelect(n-1, list)
    }
  }
  println(randomSelect(3, List('a, 'b, 'c, 'd)))

  def lotto(n:Int, end:Int) = randomSelect(n, range(1, end))
  println(lotto(6, 49))

  def randomPermute[A](xs : List[A]) : List[A] = {
    randomSelect(length(xs), xs)
  }
  println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))

  def gcd(a:Int, b:Int):Int = {
    if (a > b) gcd(b, a)
    else if (a == 0) b
    else gcd(a, b % a)
  }
  implicit class numOpertaions(i : Int) {
    def isPrime = (2 until i) forall (i % _ != 0)
    def isCoprimeTo(j:Int) = gcd(i, j) == 1
    def totient:Int = length((1 to i).filter(isCoprimeTo(_)).toList)
  }
  
  def listPrimesinRange(xs : List[Int]) : List[Int] = xs.filter(x => x.isPrime)
  println(7.isPrime)
  println(gcd(36, 63))
  println(35.isCoprimeTo(64))
  println(10.totient)
  println(listPrimesinRange((7 to 31).toList))
  
  def and(a:Boolean, b:Boolean) = a && b
  def or(a:Boolean, b:Boolean) = a || b
  def nand(a:Boolean, b:Boolean) = !and(a, b)
  def nor(a:Boolean, b:Boolean) = !or(a, b)
  def impl(a:Boolean, b:Boolean) = !a || b
  def equ(a:Boolean, b:Boolean) = and(impl(a, b), impl(b, a))
  def xor(a:Boolean, b:Boolean) = !equ(a, b)
  def table2(f:(Boolean, Boolean) => Boolean) = {
    println("A    B    result")
    for{a <- List(true, false); b <- List(true, false)} println(s"$a $b ${f(a, b)}")
  }
  table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
  
  def gray(n:Int):List[String] = {
    if (n==0) List("")
    else {
      val previousList=gray(n-1)
      previousList.map("0" + _) ++ previousList.reverse.map("1" + _)
    }
  }
  println(gray(3))
}
