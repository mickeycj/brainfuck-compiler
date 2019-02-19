/** Imports */
import scala.io.Source
import compiler.{Compiler, InvalidSyntaxException}
import machine.Machine
/** Run an example Brainfuck program => helloworld.bf */
object Main extends App {
  val fileName = "helloworld.bf"
  val bufferedSource = Source.fromResource(fileName)
  val program = bufferedSource.getLines.mkString
  val machine = new Machine(System.in, System.out)
  try {
    val operations = Compiler.compile(program)
    machine.execute(operations)
  } catch {
    case e: InvalidSyntaxException => println(e.getMessage)
    case _: Throwable => println("Unknown Exception!")
  }
  bufferedSource.close
}
