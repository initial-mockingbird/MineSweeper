package Delilah

import cats.{Foldable, Functor}
import cats.arrow.Compose
import cats.syntax.all.*
import cats.derived.*

import scala.annotation.targetName

object Algebra {
  /* Boolean Algebra with A-terms */
  sealed trait Algebra[A] derives Foldable, Functor{
    def &(other: Algebra[A]): Algebra[A]
    def |(other: Algebra[A]): Algebra[A]
    def unary_~ : Algebra[A]
    def |>(other : Algebra[A]): Algebra[A]
  }
  case class VariableTerm[A](varT : A)   extends Algebra[A]{
    def &(other: Algebra[A]): Algebra[A] = And(this, other)
    def |(other: Algebra[A]): Algebra[A] = Or(this, other)
    def unary_~ : Algebra[A] = Negate(this)
    def |>(other : Algebra[A]): Algebra[A] = ~this | other
  }
  case class Negate[A](neg : Algebra[A]) extends Algebra[A] {
    def &(other: Algebra[A]): Algebra[A] = And(this, other)
    def |(other: Algebra[A]): Algebra[A] = Or(this, other)
    def unary_~ : Algebra[A] = Negate(this)
    def |>(other : Algebra[A]): Algebra[A] = ~this | other
  }
  case class And[A](_1 : Algebra[A], _2 : Algebra[A]) extends Algebra[A] {
    def &(other: Algebra[A]): Algebra[A] = And(this, other)
    def |(other: Algebra[A]): Algebra[A] = Or(this, other)
    def unary_~ : Algebra[A] = Negate(this)
    def |>(other : Algebra[A]): Algebra[A] = ~this | other
  }
  case class Or[A] (_1 : Algebra[A], _2 : Algebra[A]) extends Algebra[A] {
    def &(other: Algebra[A]): Algebra[A] = And(this, other)
    def |(other: Algebra[A]): Algebra[A] = Or(this, other)
    def unary_~ : Algebra[A] = Negate(this)
    def |>(other : Algebra[A]): Algebra[A] = ~this | other
  }

  def flatten[A](t : Algebra[Algebra[A]]) : Algebra[A]
    = t match
    case And(_1, _2) => And(flatten(_1),flatten(_2))
    case Or(_1, _2)  => Or(flatten(_1),flatten(_2))
    case Negate(neg) => Negate(flatten(neg))
    case VariableTerm(varT) => varT

  /* Making it compositional is too much of an overkill for this project */
  def move_negate_inwards[A](term : Algebra[A]) : Algebra[A] =
    term match
      /* ~~p = p */
      case Negate(neg : Negate[A]) =>  move_negate_inwards(neg.neg)
      /* ~(p^q) = ~p v ~q*/
      case Negate(neg : And[A])    => Or(move_negate_inwards(Negate(neg._1)), move_negate_inwards(Negate(neg._2)))
      /* ~(pvq) = ~p ^ ~q*/
      case Negate(neg: Or[A])      => And(move_negate_inwards(Negate(neg._1)), move_negate_inwards(Negate(neg._2)))
      /* ~p = ~p when p is an atom*/
      case Negate(neg : VariableTerm[A]) => Negate(neg)
      /* recursive pass through cases */
      //  if _1 and _2 have their negations pushed, then _1 ^ _2 also have their negations pushed
      case And(_1,_2)              => And(move_negate_inwards(_1), move_negate_inwards(_2))
      //  if _1 and _2 have their negations pushed, then _1 v _2 also have their negations pushed
      case Or(_1,_2)               => Or(move_negate_inwards(_1), move_negate_inwards(_2))
      //  trivial base case
      case VariableTerm(_)         => term

  /* Making it compositional is too much of an overkill for this project */
  /* Assume that term has their negation pushed */
  def distribute_and_over_or[A](term : Algebra[A]) : Algebra[A] =
  term match
    case Or(_1, _2) => {
      val p = distribute_and_over_or(_1)
      val q = distribute_and_over_or(_2)
      (p,q) match
        case (And(_1,_2),_) => distribute_and_over_or(_1 | q) & distribute_and_over_or(_2 | q)
        case (_,And(_1,_2)) => distribute_and_over_or(p | _1) & distribute_and_over_or(p | _2)
        case _ => p | q
    }
    /* recursive pass through cases */
    /* remember that `distribute_and_over_or` pushes `v` inside... but also pulls `^` outside, so
    * even if we end up having: p ^ q ~ p ^ (r ^ s), we have nothing to push anymore.
    * */
    case And(_1, _2) => And(distribute_and_over_or(_1), distribute_and_over_or(_2))
    /* remember that we assume that all negations are pushed, thus ~p always unifies with ~Variable*/
    case Negate(neg : VariableTerm[A]) => Negate(distribute_and_over_or(neg))
    /*dumb base case*/
    case VariableTerm(_) => term
    case _ => {
      println(term)
      throw new IllegalArgumentException()
    }

  def to_CNF[A](t : Algebra[A]) : Algebra[A] = (move_negate_inwards[A] >>> distribute_and_over_or[A])(t)

  def flatten_CNF_And[A](t: Algebra[A]) : Seq[Algebra[A]]
    = t match
    case And(_1, _2) => flatten_CNF_And(_1) ++ flatten_CNF_And(_2)
    case _   => Seq(t)

  def flatten_CNF_Or[A](t: Algebra[A]): Seq[Algebra[A]]
  = t match
    case Or(_1, _2) => flatten_CNF_Or(_1) ++ flatten_CNF_Or(_2)
    case _ => Seq(t)

  def dinmacs_base_mapper(t : Algebra[Int]) : Int
    = t match
    case Negate(neg) => -dinmacs_base_mapper(neg)
    case VariableTerm(t) => t

  def to_dinmacs_with_count(t : Algebra[Int]) : (Int,String) = {
    val flatten_ands = flatten_CNF_And(t)
    val flatten_ors  = flatten_ands.map(it => flatten_CNF_Or(it).map(dinmacs_base_mapper))
    val xs           = flatten_ors.map(it => it.foldLeft("")((acc,x) => acc + " " + x.toString) + " 0" )
    val lines = (xs : Seq[String]) => xs.foldLeft("")((acc,b) => acc ++ "\n" ++ b)

    (xs.length, lines(xs))

  }

  def all[A,F[A] : Foldable](t : F[A]) : Algebra[A]  = {
    val l : List[A] = t.toList
    l.tail.foldLeft(VariableTerm(l.head) : Algebra[A])((acc,e) => VariableTerm(e) & acc)
  }

  def or[A, F[A] : Foldable](t: F[A]): Algebra[A] = {
    val l: List[A] = t.toList
    l.tail.foldLeft(VariableTerm(l.head): Algebra[A])((acc, e) => VariableTerm(e) | acc)
  }

  def to_dinmacs(t: Algebra[Int]): String =  to_dinmacs_with_count(t)._2

  def toPrettyString[A](t : Algebra[A]) : String = {
    t match
      case VariableTerm(varT) => varT.toString
      case Negate(VariableTerm(varT)) => "~" + varT.toString
      case Negate(Negate(neg)) => "~" + toPrettyString(Negate(neg))
      case Negate(neg) => "~(" + toPrettyString(neg) + ")"
      case And(Or(p,q), Or(r,s)) => "(" +  toPrettyString(Or(p,q)) + ")" + " ^ " + "(" +  toPrettyString(Or(r,s)) + ")"
      case And(Or(p,q), r) => "(" +  toPrettyString(Or(p,q)) + ")" + " ^ " + toPrettyString(r)
      case And(p, Or(r,s)) => toPrettyString(p)+ " ^ " + "(" +  toPrettyString(Or(r,s)) + ")"
      case And(p,q) => toPrettyString(p) + " ^ " + toPrettyString(q)
      case Or(_1, _2) => toPrettyString(_1) + " v " + toPrettyString(_2)
  }

  def len[A](t: Algebra[A]): Int = {
    var acc = Seq(t)
    var l = 0
    while (acc.length > 0) {
      val a = acc.head
      acc = acc.tail
      a match
        case And(a, b) => {
          l += 1; acc = a +: b +: acc
        }
        case Or(a, b) => {
          l += 1; acc = a +: b +: acc
        }
        case Negate(a) => {
          l += 1; acc = a +: acc
        }
        case VariableTerm(a) => {
          l += 1
        }
    }
    l
  }

}
