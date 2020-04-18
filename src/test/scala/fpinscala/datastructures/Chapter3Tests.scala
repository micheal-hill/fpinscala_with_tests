package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{GivenWhenThen, OneInstancePerTest}

class Chapter3Tests extends AnyWordSpec
  with GivenWhenThen
  with OneInstancePerTest {

  "exercise 3.1" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    assert(x == 3)
  }

  "exercise 3.3" when {
    "an empty list has no head to be set" in {
      val l = Nil
      assert(l.setHead("foo") == Nil)
    }

    "a non empty list will replace the head" when {
      "the list has one element" in {
        val l = List(1)
        assert(l.setHead(2) == List(2))
      }

      "the list has multiple elements" in {
        val l = List(1, 2, 3)
        assert(l.setHead(4) == List(4, 2, 3))
      }
    }
  }

  "exercise 3.4" when {
    "drop from an empty list does nothing" in {
      assert(Nil.drop(0) == Nil)
      assert(Nil.drop(1) == Nil)
      assert(Nil.drop(2) == Nil)
      assert(Nil.drop(5) == Nil)
    }

    "drop from a list with one element" in {
      assert(List(1).drop(0) == List(1))
      assert(List(1).drop(1) == Nil)
      assert(List(1).drop(2) == Nil)
      assert(List(1).drop(5) == Nil)
    }

    "drop from a list of multiple elements" in {
      assert(List(1, 2, 3, 4).drop(0) == List(1, 2, 3, 4))
      assert(List(1, 2, 3, 4).drop(1) == List(2, 3, 4))
      assert(List(1, 2, 3, 4).drop(2) == List(3, 4))
      assert(List(1, 2, 3, 4).drop(3) == List(4))
      assert(List(1, 2, 3, 4).drop(4) == Nil)
      assert(List(1, 2, 3, 4).drop(5) == Nil)
    }
  }

  "exercise 3.6" when {
    "init of Nil does nothing" in {
      assert(Nil.init(0) == Nil)
      assert(Nil.init(1) == Nil)
    }

    "init of a list with one element" in {
      assert(List(1).init(0) == Nil)
      assert(List(1).init(1) == List(1))
      assert(List(1).init(2) == List(1))
    }

    "init of a list with multiple elements" in {
      assert(List(1, 2, 3, 4).init(0) == Nil)
      assert(List(1, 2, 3, 4).init(1) == List(1))
      assert(List(1, 2, 3, 4).init(3) == List(1, 2, 3))
      assert(List(1, 2, 3, 4).init(4) == List(1, 2, 3, 4))
      assert(List(1, 2, 3, 4).init(5) == List(1, 2, 3, 4))
    }
  }

  "exercise 3.10 - foldLeft" in {
    assert(Nil.foldLeft("^")(_ + _) == "^")
    assert(List("a").foldLeft("^")(_ + _) == "^a")
    assert(List("a", "b").foldLeft("^")(_ + _) == "^ab")
    assert(List("a", "b", "c").foldLeft("^")(_ + _) == "^abc")
  }

  "exercise 3.12 - reverse" in {
    assert(Nil.reverse == Nil)
    assert(List(1).reverse == List(1))
    assert(List(1, 2, 3, 4).reverse == List(4, 3, 2, 1))
  }

  "exercise 3.13" when {
    "foldLeft in terms of foldRight" when {
      for (l <- Seq(Nil, List("a"), List("a", "b", "c", "d")))
        s"given $l" in {
          assert(l.foldLeft("^")(_ + _) == l.leftFolderByRight("^")(_ + _))
        }
    }
    "foldRight in terms of foldLeft" when {
      for (l <- Seq(Nil, List("a"), List("a", "b", "c", "d")))
        s"given $l" in {
          assert(l.foldRight("$")(_ + _) == l.rightFolderByLeft("$")(_ + _))
        }
    }
  }

  "exercise 3.14" when {
    "append to Nil" in {
      val l = Nil
      assert(l.append(Nil) == Nil)
      assert(l.append(List(1)) == List(1))
      assert(l.append(List(1, 2, 3)) == List(1, 2, 3))
    }

    "append to a list with one element" in {
      val l = List(9)
      assert(l.append(Nil) == List(9))
      assert(l.append(List(1)) == List(9, 1))
      assert(l.append(List(1, 2, 3)) == List(9, 1, 2, 3))
    }

    "append to a list with multiple elements" in {
      val l = List(9, 8, 7)
      assert(l.append(Nil) == List(9, 8, 7))
      assert(l.append(List(1)) == List(9, 8, 7, 1))
      assert(l.append(List(1, 2, 3)) == List(9, 8, 7, 1, 2, 3))
    }
  }

  "exercise 3.15" when {
    "flatten nothing" in {
      assert(flatten(Nil) == Nil)
      assert(flatten(List(Nil)) == Nil)
      assert(flatten(List(Nil, Nil, Nil)) == Nil)
    }

    "flatten one element" in {
      for (l <- Seq(List(1), List(1, 2, 3)))
        assert(flatten(List(l)) == l)
    }

    "flatten multiple elements" in {
      assert(flatten(List(List(1), Nil, List(2, 3), List(4, 5, 6, 7), Nil)) == List(1, 2, 3, 4, 5, 6, 7))
    }
  }

  "exercise 3.18" when {
    "map nothing" in {
      assert(List[Int]().map(_ + 1) == Nil)
    }

    "map one element" in {
      assert(List(1).map(_ + 1) == List(2))
    }

    "map multiple elements" in {
      assert(List(1, 2, 3).map(_ + 1) == List(2, 3, 4))
    }
  }

  "exercise 3.19" when {
    "filter nothing" in {
      assert(List[Int]().filter(_ > 5) == Nil)
    }

    "filter one element" in {
      assert(List(1).filter(_ > 5) == Nil)
      assert(List(6).filter(_ > 5) == List(6))
    }

    "filter multiple elements" in {
      assert(List(1, 2, 3).filter(_ > 5) == Nil)
      assert(List(5, 6).filter(_ > 5) == List(6))
      assert(List(7, 8, 9).filter(_ > 5) == List(7, 8, 9))
    }
  }

  "exercise 3.20" when {
    def fizzBuzz(i: Int) =
      if (i % 3 == 0 || i % 5 == 0) Nil
      else List(1 to i: _*)

    "flatMap nothing" in {
      assert(List[Int]().flatMap(_ => List(1, 2, 3)) == Nil)
      assert(List[Int]().flatMap(fizzBuzz) == Nil)
    }

    "flatMap one element" in {
      assert(List(1).flatMap(fizzBuzz) == List(1))
      assert(List(2).flatMap(fizzBuzz) == List(1, 2))
      assert(List(3).flatMap(fizzBuzz) == Nil)
      assert(List(4).flatMap(fizzBuzz) == List(1, 2, 3, 4))
    }

    "flatMap multiple elements" in {
      assert(List(1, 2).flatMap(fizzBuzz) == List(1, 1, 2))
      assert(
        List(2, 5, 8).flatMap(fizzBuzz) ==
          List(
            1, 2,
            1, 2, 3, 4, 5, 6, 7, 8
          )
      )
      assert(List(3, 5, 10).flatMap(fizzBuzz) == Nil)
    }
  }

  "exercise 3.23" when {
    "both lists have the same length" in {
      assert(List[Int]().zipWith(List[Int]())(_ + _) == Nil)
      assert(List(1).zipWith(List(2))(_ + _) == List(3))
      assert(List(4, 5, 6).zipWith(List(7, 8, 9))(_ + _) == List(11, 13, 15))
    }

    "one list is short" in {
      assert(List[Int]().zipWith(List(1, 2, 3))(_ + _) == Nil)
      assert(List(1).zipWith(List(1, 2, 3))(_ + _) == List(2))
      assert(List(1, 2).zipWith(List(1, 2, 3))(_ + _) == List(2, 4))

      assert(List(1, 2, 3).zipWith(List[Int]())(_ + _) == Nil)
      assert(List(1, 2, 3).zipWith(List(1))(_ + _) == List(2))
      assert(List(1, 2, 3).zipWith(List(1, 2))(_ + _) == List(2, 4))
    }
  }

  "exercise 3.24" when {
    val t = Table(
      // @formatter:off
      ("seq",                  "possibleSubSeq", "isSubSeq"),
      (Nil,                    Nil,                    true),
      (Nil,                    List(1),               false),
      (Nil,                    List(1, 2, 3),         false),
      (List(1),                Nil,                    true),
      (List(1),                List(1),                true),
      (List(1),                List(1, 2, 4),         false),
      (List(1, 2, 3),          Nil,                    true),
      (List(1, 2, 3),          List(1),                true),
      (List(1, 2, 3),          List(1, 2, 3),          true),
      (List(1, 1, 2, 2, 3, 3), Nil,                    true),
      (List(1, 1, 2, 2, 3, 3), List(1),                true),
      (List(1, 1, 2, 2, 3, 3), List(1, 2, 3),         false),
      // @formatter:on
    )

    forAll(t) { (seq, possibleSubSeq, isSubSeq) =>
      s"$possibleSubSeq should ${if (isSubSeq) "" else "not "} be a subsequence of $seq" in {
        assert(seq.hasSubSequence(possibleSubSeq) == isSubSeq)
      }
    }
  }
}
