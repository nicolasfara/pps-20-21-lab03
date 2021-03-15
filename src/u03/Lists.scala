package u03

import scala.annotation.tailrec
import u02.Optionals._
import u02.SumTypes.{Person, Student, Teacher}

object Lists {

  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object List {
    case class Cons[E](head: E, tail: List[E]) extends List[E]
    case class Nil[E]() extends List[E]

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def map[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()
    }

    def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
      case Cons(h,t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_,t) => filter(t)(pred)
      case Nil() => Nil()
    }

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_, tail) if n > 0 => drop(tail, n - 1)
      case Cons(head, tail) => Cons(head, tail)
      case _ => Nil()
    }

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Cons(head, tail) => append(f(head), flatMap(tail)(f))
      case _ => Nil()
    }

    def mapFlatMap[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(e => Cons(mapper(e), Nil()))

    def filterFlatMap[A, B](l: List[A])(pred: A => Boolean): List[A] = flatMap(l) {
      case e if pred(e) => Cons(e, Nil())
      case _ => Nil()
    }

    def max(l: List[Int]): Option[Int] = {
      @tailrec
      def _max(list: List[Int], currMax: Int): Int = list match {
        case Cons(head, tail) if head > currMax => _max(tail, head)
        case Cons(head, tail) if head <= currMax => _max(tail, currMax)
        case Nil() => currMax
      }

      l match {
        case Cons(_, _) => Option.Some(_max(l, Int.MinValue))
        case Nil() => Option.None()
      }
    }

    def coursesByTeachers(l: List[Person]): List[String] = map(filter(l) {
      case _: Student => false
      case _ => true
    })(e => e.asInstanceOf[Teacher].course)
  }
}

object ListsMain extends App {
  import Lists._
  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60
  import List._
  import u03.Lists.List
  println(append(Cons(5, Nil()), l)) // 5,10,20,30
  println(filter[Int](l)(_ >=20)) // 20,30

  println(drop(l, 1)) // 20,30
  println(drop(l, 2)) // 30
  println(drop(l, 3)) // Nil()

  println(flatMap(l)(v => Cons(v+1, Nil()))) //11, 21, 31

  println(mapFlatMap(l)(_ + 1)) // 11, 21, 31
  println(filterFlatMap(l)(_ >= 20)) // 20, 30

  println(max(l))
  println(max(Nil[Int]()))

  val personList: List[Person] = Cons(Student("mario", 2012), Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil())))
  println(coursesByTeachers(personList))
}