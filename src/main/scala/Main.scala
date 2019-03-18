/** Imports */
import scala.io.Source
import scala.util.{Failure, Success, Try}
import compiler.Compiler
import machine.Machine
/** Run an example Brainfuck program => hello.bf */
object Main extends App {
  val fileName = "hello.bf"
  Try(Source.fromResource(fileName)) match {
    case Success(bufferedSource) =>
      val program = bufferedSource.getLines.mkString
      val machine = new Machine(System.in, System.out)
      Try(machine.execute(Compiler.compile(program))) match {
        case Success(_) => machine.clear()
        case Failure(f) => println(f.getMessage)
      }
      bufferedSource.close()
    case Failure(f) => println(f.getMessage)
  }
}
