package machine
/** Imports */
import java.io.{InputStream, PrintStream}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSuite
import org.scalatest.mockito.MockitoSugar
/** MachineSpec
 *
 *  Test suite for Machine class.
 */
class MachineSpec extends FunSuite with BeforeAndAfter with MockitoSugar {
  /** Setup codes */
  val in: InputStream = mock[InputStream]
  val out: PrintStream = mock[PrintStream]
  val tapes: Array[Int] = Array.fill[Int](10){0}
  var machine: Machine = _
  before {
    machine = new Machine(in, out, tapes)
  }
  /** Tests for all BrainFuck's operations.
   *
   *  Each function must perform according to BrainFuck's specifications.
   */
  test("Increment Pointer - Case 1:\nTape pointer at 0 should move to 1 after one function call.") {
    machine.incrementPointer(1)
    assert(machine.tapePointer == 1)
  }
  test("Increment Pointer - Case 2:\nTape pointer at 127 should move to 0 after one function call.") {
    machine.tapePointer = 127
    machine.incrementPointer(1)
    assert(machine.tapePointer == 0)
  }
  test("Decrement Pointer - Case 1:\nTape pointer at 0 should move to 127 after one function call.") {
    machine.decrementPointer(1)
    assert(machine.tapePointer == 127)
  }
  test("Decrement Pointer - Case 2:\nTape pointer at 127 should move to 126 after one function call.") {
    machine.tapePointer = 127
    machine.decrementPointer(1)
    assert(machine.tapePointer == 126)
  }
}
