package Delilah

import Delilah.Algebra.*
import Delilah.Encodings.Position
import cats.arrow.Compose
import cats.syntax.all.*

import java.io.{File, FileOutputStream, FileWriter}
import scala.io.Source
import scala.collection.mutable
import java.time.LocalDate
import java.time.LocalTime
import java.time.temporal.ChronoUnit
import scala.collection.mutable.ListBuffer

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
    val file = new FileWriter(new File(fp))
    m.map(
      line => {
        line.map {
          case Mine() => 'X'
          case Blank() => 'B'
          case Unplayable() => 'U'
          case Number(n) => n.toString.charAt(0)
        }
      }
    ).foreach(line => {
      file.write(line.appended('\n').toArray)
    })
    file.close
  }

  // opens a file, reads a map
  def from_file(fp : String) : Map = {
    val buffer = Source.fromFile(fp)
    val seqs = buffer.getLines().map(line => {
      line.map {
        case 'X' => Mine()
        case 'B' => Blank()
        case 'U' => Unplayable()
        case c => {
          if (!Character.isDigit(c))
            throw new IllegalArgumentException("Wrong map representation")
          Number(c.toString.toInt)
        }
      }
    })

    seqs.toSeq
  }

  // opens a dinmacs file, returns an error message if NOSAT else returns
  // a sequence with the dinmacs vars
  def _from_dinmacs_file(fp : String) : Either[String,Seq[DinMacsVar]] = {
    val buffer = Source.fromFile(fp)
    val line = buffer.getLines().toSeq.apply(0)

    line match
      case "UNSAT" => Left("Tablero ambiguo")
      case _ => {
        Right(line.split(" ").map(s => (s.toInt - 1)).filter(s => s > 0).toSeq.dropRight(0))
      }
  }

  def _from_dinmacs_file(m : Map)(fp: String): Either[String, Seq[MineEncoding]] = {
    _from_dinmacs_file(fp).map(it => it.map(cnf_var_to_mine_encoding(m)))
  }

  // reads dinmacs file into either a map or an error
  def from_dinmacs_file(m: Map)(fp: String): Either[String,Map] = {
    _from_dinmacs_file(m)(fp).map(mines => set_mines_unsafe(m)(mines))
  }

  def mine_encoding_to_cnf_var(m : Map)(mineEncoding: MineEncoding) : DinMacsVar = {
    val cols  = m.head.length
    val (i,j) = mineEncoding
    i * cols + j + 1
  }

  def cnf_var_to_mine_encoding(m : Map)(cnfVar : DinMacsVar) : MineEncoding = {
    val cols = m.head.length
    val v    = cnfVar - 1
    val j    = cnfVar % cols
    val i    = cnfVar / cols
    (i,j)
  }

  /** ********** */
  /** Functions **/
  /** ********** */

  // sets the given mines without checking conditions
  def set_mines_unsafe(m : Map)(mines : Seq[MineEncoding]) : Map = {
    var out = m
    mines.foreach(mine =>
      out = out.updated(mine._1, out.apply(mine._1).updated(mine._2, Mine()))
    )
    out
  }

  // true if the element can be replaced by a mine, false otherwise
  def is_mineable(mapElement: MapElement) : Boolean = {
    mapElement match
      case Blank() => true
      case _ => false
  }

  // Return a set of (where a Number is, what value does it hold)
  def find_numbers(m: Map): Set[(Position, Int)] = {
    m.zipWithIndex.map(trow => {
      trow._1.zipWithIndex.map(ttile => {
        ttile._1 match
          case Number(n) => ((trow._2, ttile._2), n)
          case _ => ((-1, -1), -1)
      }).filter(r => r._2 != -1)
    }).fold(Seq.empty)((a, b) => a.concat(b)).toSet
  }

  // returns the neighborhood of a given position
  def neighborhood(m: Map)(p: Position): Set[(Position, MapElement)] = {
    val w = m.apply(0).length
    val h = m.length

    (for i <- -1 until 2
         j <- -1 until 2 if ((i != 0 || j != 0) && (0 until h contains (p._1 + i)) && (0 until w contains (p._2 + j)))
    yield ((p._1 + i, p._2 + j), m.apply(p._1 + i).apply(p._2 + j))).toSet
  }

  // returns all the positions of a neighborhood that a mine can be positioned.
  def mineable_neighborhood(m : Map)(p : Position) : Set[Position]
    = neighborhood(m)(p).filter(ab => is_mineable(ab._2)).map(_._1)

  // Give mines and the neighborhood, yields restriction
  def encode_space1[A](total : Set[A])(mines : Set[A]) : Algebra[A]
    = all(mines.toSeq) & flatten(all(total.diff(mines).toSeq).map(it => ~VariableTerm(it)))

  // Give mines and no mines, yields restriction
  def encode_space2[A](mines: Set[A], noMines: Set[A]): Algebra[A]
    = all(mines.toSeq) & flatten(all(noMines.toSeq).map(it => ~VariableTerm(it)))

  // combine everything here
  def to_dinmacs_str(m : Map) : String = {
    // your goal is to generate something of this type Algebra[DinMacsVar]
    // hint: use enconde_space1 or encode_space2
    val neigh_m  = neighborhood(m)
    val algebra1  :  Seq[Algebra[MineEncoding]]
      =  find_numbers(m).map( n => {
      val mn = mineable_neighborhood(m)(n._1).toSeq
      val neigh_n = neigh_m(n._1).map(_._1)
      val mineCombinations = mn.combinations(n._2).toSeq
      val FF_n = or(mineCombinations.map( ((it : Seq[Position] )=> it.toSet) >>> encode_space1(neigh_n)))
      val F_n  = flatten (FF_n)
      F_n
    }).toSeq

    val algebra2 : Algebra[DinMacsVar] = flatten(all(algebra1)).map(mine_encoding_to_cnf_var(m))
    val a : Algebra[DinMacsVar] = to_CNF(algebra2)
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

