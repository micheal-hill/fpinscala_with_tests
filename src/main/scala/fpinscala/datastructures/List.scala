package fpinscala.datastructures

sealed trait List[+A] {
  def tail: List[A]

  def setHead[B >: A](newHead: B): List[B]

  def drop(i: Int): List[A]

  def init(i: Int): List[A]

  def foldLeft[B](z: B)(f: (B, A) => B): B

  def reverse: List[A]

  def leftFolderByRight[B](initial: B)(f: (B, A) => B): B = {
    val foldedFnChain = this.foldRight((agg: B) => agg)((element, nextInChain) => { agg =>
      val nextAgg = f(agg, element)
      nextInChain(nextAgg)
    })

    foldedFnChain(initial)
  }

  def append[B >: A](other: List[B]): List[B] = this.foldRight(other)(Cons(_, _))

  def foldRight[B]: B => ((A, B) => B) => B = List.foldRight(this, _)

  def length: Int = this.foldLeft(0)((agg, _) => agg + 1)

  def map[B](f: A => B): List[B] = this.rightFolderByLeft[List[B]](Nil)((e, agg) => Cons(f(e), agg))

  def rightFolderByLeft[B](initial: B)(f: (A, B) => B): B = {
    val foldedFnChain = this.foldLeft((agg: B) => agg)((nextInChain, element) => { agg =>
      val nextAgg = f(element, agg)
      nextInChain(nextAgg)
    })

    foldedFnChain(initial)
  }

  def filter(f: A => Boolean): List[A] = this.rightFolderByLeft[List[A]](Nil) { (e, agg) =>
    if (f(e))
      Cons(e, agg)
    else
      agg
  }

  def flatMap[B](f: A => List[B]): List[B] = this.rightFolderByLeft[List[B]](Nil) { (e, agg) =>
    f(e).append(agg)
  }

  def zipWith[B, C](other: List[B])(f: (A, B) => C): List[C] = (this, other) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(lHead, lTail), Cons(rHead, rTail)) =>
      Cons(f(lHead, rHead), lTail.zipWith(rTail)(f))
  }

  def hasSubSequence[B >: A](other: List[B]): Boolean
}

case object Nil extends List[Nothing] {
  override def drop(i: Int): List[Nothing] = Nil

  override def init(i: Int): List[Nothing] = Nil

  override def tail: List[Nothing] = Nil

  override def setHead[B >: Nothing](newHead: B): List[B] = Nil

  override def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  override def reverse: List[Nothing] = Nil

  def hasSubSequence[B >: Nothing](other: List[B]): Boolean = other == Nil
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def setHead[B >: A](newHead: B): List[B] = Cons(newHead, tail)

  override def drop(i: Int): List[A] = {
    if (i <= 0) this
    else tail.drop(i - 1)
  }

  override def init(i: Int): List[A] = {
    if (i <= 0) Nil
    else Cons(head, tail.init(i - 1))
  }

  override def reverse: List[A] = foldLeft[List[A]](Nil)((agg, e) => Cons(e, agg))

  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    val aggResult = f(z, head)
    tail.foldLeft(aggResult)(f)
  }

  override def hasSubSequence[B >: A](that: List[B]): Boolean = {
    def loop: (List[B], List[B]) => Boolean = {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(lHead, lTail), Cons(rHead, _)) if lHead != rHead => loop(lTail, that)
      case (Cons(_, lTail), Cons(_, rTail)) =>
        loop(lTail, rTail) || loop(lTail, that)
    }

    loop(this, that)
  }
}

object List extends GivenImplementationsForObject {
  def flatten[A](lists: List[List[A]]): List[A] = lists match {
    case Nil => Nil
    case Cons(head, tail) => head.append(flatten(tail))
  }
}

trait GivenImplementationsForObject {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
}