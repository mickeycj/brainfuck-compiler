package machine
/** Imports */
import java.io.{IOException, InputStream, PrintStream}
import compiler.Operation
/** Class for running BrainFuck's compiled codes.
 *
 *  This class deals with reading BrainFuck's files and executing compiled codes.
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
  def incrementPointer(positions: Int): Unit = {
    tapePointer += positions
    if (tapePointer >= tapes.length) {
      tapePointer %= tapes.length
    }
  }
  /** Decrement the pointer's position.
   *
   *  @param positions the number of positions to move
   */
  def decrementPointer(positions: Int): Unit = {
    tapePointer -= positions
    if (tapePointer < 0) {
      tapePointer += tapes.length
    }
  }
  /** Increment the value at the current tape position.
   *
   *  @param value the value to increment
   */
  def incrementValue(value: Int): Unit = {
    tapes(tapePointer) += value
    if (tapes(tapePointer) >= tapeSize) {
      tapes(tapePointer) %= tapeSize
    }
  }
  /** Decrement the value at the current tape position.
   *
   *  @param value the value to decrement
   */
  def decrementValue(value: Int): Unit = {
    tapes(tapePointer) -= value
    if (tapes(tapePointer) < 0) {
      tapes(tapePointer) += tapeSize
    }
  }
  /** Read the input from the input stream.
   *
   *  @param times the number of times to read
   */
  @throws(classOf[IOException])
  def read(times: Int): Unit = { }
  /** Print to the output stream.
   *
   *  @param times the number of times to print
   */
  def print(times: Int): Unit = { }
  /** Jump to the specified instruction.
   *
   *  @param position the next instruction pointer position
   */
  def jump(position: Int): Unit = {
    instructionPointer = position
  }
  /** Execute the specified sequence of operations.
   *
   *  @param operations the operations to be executed
   */
  @throws(classOf[IOException])
  def execute(operations: Seq[Operation]): Unit = { }
  /** Clear the tapes.
   *
   */
  def clear(): Unit = {
    tapes = Array.fill[Int](tapes.length){0}
    tapePointer = 0
    instructionPointer = 0
  }
}
