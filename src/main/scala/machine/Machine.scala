package machine
/** Imports */
import java.io.{IOException, InputStream, PrintStream}
import scala.annotation.tailrec
import compiler.{Instruction, InvalidSyntaxException}
/** Class for running Brainfuck's compiled codes.
 *
 *  This class deals with reading Brainfuck's files and executing compiled codes.
 */
class Machine(
  val in: InputStream,
  val out: PrintStream,
  var tapes: Array[Int] = Array.fill[Int](Short.MaxValue){0},
  val tapeSize: Int = 128,
  var tapePointer: Int = 0,
  var instructionPointer: Int = 0
) {
  /** Increment the pointer's position.
   *
   *  @param positions the number of positions to move
   */
  final def incrementPointer(positions: Int): Unit = {
    tapePointer += positions
    if (tapePointer >= tapes.length) {
      tapePointer %= tapes.length
    }
  }
  /** Decrement the pointer's position.
   *
   *  @param positions the number of positions to move
   */
  final def decrementPointer(positions: Int): Unit = {
    tapePointer -= positions
    if (tapePointer < 0) {
      tapePointer += tapes.length
    }
  }
  /** Increment the value at the current tape position.
   *
   *  @param value the value to increment
   */
  final def incrementValue(value: Int): Unit = {
    tapes(tapePointer) += value
    if (tapes(tapePointer) >= tapeSize) {
      tapes(tapePointer) %= tapeSize
    }
  }
  /** Decrement the value at the current tape position.
   *
   *  @param value the value to decrement
   */
 final def decrementValue(value: Int): Unit = {
    tapes(tapePointer) -= value
    if (tapes(tapePointer) < 0) {
      tapes(tapePointer) += tapeSize
    }
  }
  /** Print to the output stream.
   *
   *  @param times the number of times to print
   */
  @tailrec
  final def printChar(times: Int): Unit = {
    out.print(tapes(tapePointer).toChar)
    times match {
      case 1 => 
      case _ => printChar(times - 1)
    }
  }
  /** Read the input from the input stream.
   *
   *  @param times the number of times to read
   */
  @tailrec
  @throws(classOf[IOException])
  final def readChar(times: Int): Unit = {
    tapes(tapePointer) = in.read
    times match {
      case 1 =>
      case _ => readChar(times - 1)
    }
  }
  /** Jump to the specified instruction.
   *
   *  @param position the next instruction pointer position
   */
  final def jump(position: Int): Unit = {
    instructionPointer = position
  }
  /** Execute the specified sequence of operations.
   *
   *  @param operations the operations to be executed
   */
  @tailrec
  @throws(classOf[IOException])
  @throws(classOf[InvalidSyntaxException])
  final def execute(operations: Seq[Operation]): Unit = {
    val Length = operations.length
    instructionPointer match {
      case Length =>
      case _ => {
        val operation = operations(instructionPointer)
        var jumped = false
        operation.instruction match {
          case Instruction.INC_PTR => incrementPointer(operation.argument)
          case Instruction.DEC_PTR => decrementPointer(operation.argument)
          case Instruction.INC_VAL => incrementValue(operation.argument)
          case Instruction.DEC_VAL => decrementValue(operation.argument)
          case Instruction.PRINT => printChar(operation.argument)
          case Instruction.READ => readChar(operation.argument)
          case Instruction.OPEN_BRACKET => jumped = tapes(tapePointer) == 0
          case Instruction.CLOSED_BRACKET => jumped = tapes(tapePointer) != 0
          case _ => throw new InvalidSyntaxException("Syntax Error: operation does not exist!")
        }
        if (jumped) {
          jump(operation.argument)
        } else {
          instructionPointer += 1
        }
        execute(operations)
      }
    }
  }
  /** Clear the tapes.
   *
   */
  final def clear(): Unit = {
    tapes = Array.fill[Int](tapes.length){0}
    tapePointer = 0
    instructionPointer = 0
  }
}
