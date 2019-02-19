package machine
/** Imports */
import java.io.{IOException, InputStream, PrintStream}
/** Class for running BrainFuck's compiled codes.
 *
 *  This class deals with reading BrainFuck's files and executing compiled codes.
 */
class Machine(
  val in: InputStream,
  val out: PrintStream,
  val tapes: Array[Int] = Array.fill[Int](Short.MaxValue){0},
  var tapePointer: Int = 0,
  var instructionPointer: Int = 0
) {
  /** Increment the pointer's position.
   *
   *  @param positions the number of positions to move
   */
  def incrementPointer(positions: Int): Unit = { }
  /** Decrement the pointer's position.
   *
   *  @param positions the number of positions to move
   */
  def decrementPointer(positions: Int): Unit = { }
  /** Increment the value at the current tape position.
   *
   *  @param value the value to increment
   */
  def incrementValue(value: Int): Unit = { }
  /** Decrement the value at the current tape position.
   *
   *  @param value the value to decrement
   */
  def decrementValue(value: Int): Unit = { }
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
  def jump(position: Int): Unit = { }
  /** Clear the tapes.
   *
   */
  def clear(): Unit = { }
}
