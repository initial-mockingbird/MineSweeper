package Delilah

import Delilah.Algebra.*
import cats.arrow.Compose
import cats.syntax.all.*

import scala.collection.mutable
import java.time.LocalDate
import java.time.LocalTime
import java.time.temporal.ChronoUnit

object Encodings {

  /***********/
  /** TYPES **/
  /***********/

  sealed trait MapElement

  case class Number(n : Int)   extends MapElement
  case class Unplayable()      extends MapElement
  case class Mine()            extends MapElement
  case class Blank()           extends MapElement

  type Map = Seq[Seq[MapElement]]
  // (i,j)
  type MineEncoding = (Int,Int)
  // (i,j)
  type Position     = (Int,Int)

  // Dinmacs variables are integers, so that
  type DinMacsVar = Int


  /** ********** */
  /** Parsers   **/
  /** ********** */

  // writes a map to a file
  def to_file(m : Map)(fp : String) : Unit = {
    throw new NotImplementedError()
  }

  // opens a file, reads a map
  def from_file(fp : String) : Map = {
    throw new NotImplementedError()
  }

  // opens a dinmacs file, returns an error message if NOSAT else returns
  // a sequence with the dinmacs vars
  def _from_dinmacs_file(fp : String) : Either[String,Seq[DinMacsVar]] = {
    throw new NotImplementedError()
  }

  def _from_dinmacs_file(m : Map)(fp: String): Either[String, Seq[MineEncoding]] = {
    _from_dinmacs_file(fp).map(it => it.map(cnf_var_to_mine_encoding(m)))
  }

  // reads dinmacs file into either a map or an error
  def from_dinmacs_file(m: Map)(fp: String): Either[String,Map] = {
    throw new NotImplementedError()
  }

  def mine_encoding_to_cnf_var(m : Map)(mineEncoding: MineEncoding) : DinMacsVar = {
    val cols  = m.head.length
    val (i,j) = mineEncoding
    i * cols + j
  }

  def cnf_var_to_mine_encoding(m : Map)(cnfVar : DinMacsVar) : MineEncoding = {
    val cols = m.head.length
    val j    = cnfVar % cols
    val i    = cnfVar / cols
    (i,j)
  }

  /** ********** */
  /** Functions **/
  /** ********** */

  // sets the given mines without checking conditions
  def set_mines_unsafe(m : Map)(mines : Seq[MineEncoding]) : Mine = {
    throw new NotImplementedError()
  }

  // true if the element can be replaced by a mine, false otherwise
  def is_mineable(mapElement: MapElement) : Boolean = {
    throw new NotImplementedError()
  }

  // Return a set of (where a mine is, what number does it hold)
  def find_numbers(m : Map) : Set[(MineEncoding,Int)] = {
    throw new NotImplementedError()
  }

  // returns the neighborhood of a given position
  def neighborhood(m : Map)(p : Position) : Set[(Position,MapElement)] = {
    throw new NotImplementedError()
  }

  // returns all the positions of a neighborhood that a mine can be positioned.
  def mineable_neighborhood(m : Map)(p : Position) : Set[Position]
    = neighborhood(m)(p).filter(ab => is_mineable(ab._2)).map(_._1)

  // Give mines and the neighborhood, yields restriction
  def encode_space1[A](mines : Set[A], total : Set[A]) : Algebra[A]
    = all(mines.toSeq) & flatten(all(total.diff(mines).toSeq).map(it => ~VariableTerm(it)))

  // Give mines and no mines, yields restriction
  def encode_space2[A](mines: Set[A], noMines: Set[A]): Algebra[A]
    = all(mines.toSeq) & flatten(all(noMines.toSeq).map(it => ~VariableTerm(it)))

  // combine everything here
  def to_dinmacs_str(m : Map) : String = {

    // your goal is to generate something of this type Algebra[DinMacsVar]
    // hint: use enconde_space1 or encose_space2
    val a : Algebra[DinMacsVar] = throw new NotImplementedError()
    _to_dinmacs_str(m)(a)
  }
  // algebra should be in cnf.
  def _to_dinmacs_str(m : Map)(a : Algebra[DinMacsVar]) : String = {
    val N  = m.length
    val M  = m.head.length
    val (total_clauses : Int,clauses : String) = to_dinmacs_with_count(a)
    val total_vars = N * M
    "p cnf " + total_vars.toString + " " + total_clauses.toString + " \n" + clauses
  }

}

