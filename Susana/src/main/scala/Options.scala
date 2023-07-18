import cats.syntax.either.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.yaml
import Delilah.Algebra.*
import Delilah.Encodings.*

import scala.io.Source
import cats.arrow.Compose
import cats.implicits.*
import cats.syntax.all.*
import io.circe.parser.decode
import io.circe.generic.auto.*
import io.circe.syntax.*
import java.io.{File, FileOutputStream, FileWriter}
import scala.io.Source
import scala.sys.process.*
object Options {

  case class Options
  (  entryPoint: String
   , glucoseExecutable: String
   , glucoseInput: String
   , glucoseOutput: String
   , output: String
  )

  def runner() = {

    val m = Seq(
      Seq(Number(0), Mine(), Mine()),
      Seq(Number(1), Number(3), Number(5)),
      Seq(Number(2), Blank(), Number(4)))

    find_numbers(m).foreach(n => println(n))

    println(neighborhood(m)((2,2)))

    /*
    val fp = "./options.yaml"
    val readFile = (fp: String) => Source.fromFile(fp).mkString
    val options : Options = (readFile >>> yaml.parser.parse >>> (it => it.toOption.get.toString) >>> decode[Options])(fp).toOption.get
    val map = from_file(options.entryPoint)
    val glucoseInputW = new FileWriter(new File(options.glucoseInput))
    val dinmacs = to_dinmacs_str(map)
    glucoseInputW.write(dinmacs)
    glucoseInputW.close()
    println(options.glucoseExecutable + " " + options.glucoseInput + " " + options.glucoseOutput)
    (options.glucoseExecutable + " " + options.glucoseInput + " " + options.glucoseOutput).!!
    val dinmacsSol = from_dinmacs_file(map)(options.glucoseOutput).getOrElse(throw new IllegalArgumentException("UNSAT"))
    to_file(dinmacsSol)(options.output)
    */
  }
}