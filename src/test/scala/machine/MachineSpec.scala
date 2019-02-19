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
  var machine: Machine = _
  before {
    machine = new Machine(in, out, Array.fill[Int](10){0})
  }
  /** Tests for all BrainFuck's operations.
   *
   *  Each function must perform according to BrainFuck's specifications.
   */
  test("Increment Pointer - Case 1:\nTape pointer at 0 should move to 1 after one function call.") {
    machine.incrementPointer(1)
    assert(machine.tapePointer == 1)
  }
  test("Increment Pointer - Case 2:\nTape pointer at the last tape should move to 0 after one function call.") {
    machine.tapePointer = machine.tapes.length - 1
    machine.incrementPointer(1)
    assert(machine.tapePointer == 0)
  }
  test("Decrement Pointer - Case 1:\nTape pointer at 0 should move to the last tape after one function call.") {
    machine.decrementPointer(1)
    assert(machine.tapePointer == machine.tapes.length - 1)
  }
  test("Decrement Pointer - Case 2:\nTape pointer at 1 should move to 0 after one function call.") {
    machine.tapePointer = 1
    machine.decrementPointer(1)
    assert(machine.tapePointer == 0)
  }
  test("Increment Value - Case 1:\nValue at the current tape (value of 0) should be 1 after one function call.") {
    machine.incrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 1)
  }
  test("Increment Value - Case 2:\nValue at the current tape (value of 127) should be 0 after one function call.") {
    machine.tapes(machine.tapePointer) = 127
    machine.incrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 0)
  }
  test("Decrement Value - Case 1:\nValue at the current tape (value of 0) should be 127 after one function call.") {
    machine.decrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 127)
  }
  test("Decrement Value - Case 2:\nValue at the current tape (value of 1) should be 0 after one function call.") {
    machine.tapes(machine.tapePointer) = 1
    machine.decrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 0)
  }
  test("Print to OutputStream:\nShould print 'H' to OutputStream.") {
    machine.tapes(machine.tapePointer) = 72
    machine.print(1)
    verify(out).print('H')
    verifyNoMoreInteractions(out)
  }
  test("Read from InputStream:\nShould read 72 ('H') from InputStream and write to the current tape.") {
    when(in.read).thenReturn(72)
    machine.read(1)
    assert(machine.tapes(machine.tapePointer) == 72)
    verify(in).read
    verifyNoMoreInteractions(in)
  }
}
