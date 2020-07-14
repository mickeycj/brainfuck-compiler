package machine
/* Imports */
import java.io.{InputStream, PrintStream}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSuite
import org.scalatest.mockito.MockitoSugar
import compiler.Instruction
/** MachineSpec
 *
 *  Test suite for Machine class.
 */
class MachineSpec extends FunSuite with BeforeAndAfter with MockitoSugar {
  /* Setup codes */
  val in: InputStream = mock[InputStream]
  val out: PrintStream = mock[PrintStream]
  var machine: Machine = _
  before {
    reset(in)
    reset(out)
    machine = new Machine(in, out, Array.fill[Int](10){0})
  }
  /** Tests for all Brainfuck's operations.
   *
   *  Each function must perform according to Brainfuck's specifications.
   */
  test("Increment Pointer - Case 1:\nTape pointer at 0 should move to 1 after one function call.") {
    machine.incrementPointer(1)
    assert(machine.tapePointer == 1)
  }
  test("Increment Pointer - Case 2:\nTape pointer at 0 should move to 2 after two function calls.") {
    machine.incrementPointer(2)
    assert(machine.tapePointer == 2)
  }
  test("Increment Pointer - Case 3:\nTape pointer at the last tape should move to 0 after one function call.") {
    machine.tapePointer = machine.tapes.length - 1
    machine.incrementPointer(1)
    assert(machine.tapePointer == 0)
  }
  test("Decrement Pointer - Case 1:\nTape pointer at 0 should move to the last tape after one function call.") {
    machine.decrementPointer(1)
    assert(machine.tapePointer == machine.tapes.length - 1)
  }
  test("Decrement Pointer - Case 2:\nTape pointer at 0 should move to the second-to-last tape after two function calls.") {
    machine.decrementPointer(2)
    assert(machine.tapePointer == machine.tapes.length - 2)
  }
  test("Decrement Pointer - Case 3:\nTape pointer at 1 should move to 0 after one function call.") {
    machine.tapePointer = 1
    machine.decrementPointer(1)
    assert(machine.tapePointer == 0)
  }
  test("Increment Value - Case 1:\nValue at the current tape (value of 0) should be 1 after one function call.") {
    machine.incrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 1)
  }
  test("Increment Value - Case 2:\nValue at the current tape (value of 0) should be 2 after two function calls.") {
    machine.incrementValue(2)
    assert(machine.tapes(machine.tapePointer) == 2)
  }
  test("Increment Value - Case 3:\nValue at the current tape (value of 127) should be 0 after one function call.") {
    machine.tapes(machine.tapePointer) = 127
    machine.incrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 0)
  }
  test("Decrement Value - Case 1:\nValue at the current tape (value of 0) should be 127 after one function call.") {
    machine.decrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 127)
  }
  test("Decrement Value - Case 2:\nValue at the current tape (value of 0) should be 126 after one function call.") {
    machine.decrementValue(2)
    assert(machine.tapes(machine.tapePointer) == 126)
  }
  test("Decrement Value - Case 3:\nValue at the current tape (value of 1) should be 0 after one function call.") {
    machine.tapes(machine.tapePointer) = 1
    machine.decrementValue(1)
    assert(machine.tapes(machine.tapePointer) == 0)
  }
  test("Print to OutputStream - Case 1:\nShould print 1 'H' to OutputStream.") {
    machine.tapes(machine.tapePointer) = 72
    machine.printChar(1)
    verify(out).print('H')
    verifyNoMoreInteractions(out)
  }
  test("Print to OutputStream - Case 2:\nShould print 2 'H's to OutputStream.") {
    machine.tapes(machine.tapePointer) = 72
    machine.printChar(2)
    verify(out, times(2)).print('H')
    verifyNoMoreInteractions(out)
  }
  test("Read from InputStream - Case 1:\nShould read a value from InputStream and write 72 ('H') to the current tape.") {
    when(in.read)
      .thenReturn(72)
    machine.readChar(1)
    assert(machine.tapes(machine.tapePointer) == 72)
    verify(in).read
    verifyNoMoreInteractions(in)
  }
  test("Read from InputStream - Case 2:\nShould read two values from InputStream and write 72 ('H') to the current tape.") {
    when(in.read)
      .thenReturn(71)
      .thenReturn(72)
    machine.readChar(2)
    assert(machine.tapes(machine.tapePointer) == 72)
    verify(in, times(2)).read
    verifyNoMoreInteractions(in)
  }
  /** Tests for Brainfuck's code execution.
   *
   *  Each sequence of operations must perform correctly according to Brainfuck's specifications.
   */
  test("Tapes Clearing:\nTapes must be cleared when 'clear' method is called (including pointers).") {
    machine.tapes = Array.fill[Int](10){1}
    machine.tapePointer = 5
    machine.instructionPointer = 1
    machine.clear
    assert(machine.tapes.deep == Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).deep)
    assert(machine.tapePointer == 0)
    assert(machine.instructionPointer == 0)
  }
  test("Execute Increment Pointer - Case 1:\nTape pointer at 0 should move to 1 after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.INC_PTR, 1)
      )
    )
    assert(machine.tapePointer == 1)
  }
  test("Execute Increment Pointer - Case 2:\nTape pointer at 0 should move to 2 after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.INC_PTR, 2)
      )
    )
    assert(machine.tapePointer == 2)
  }
  test("Execute Increment Pointer - Case 3:\nTape pointer at the last tape should move to 0 after the execution.") {
    machine.tapePointer = machine.tapes.length - 1
    machine.execute(
      Seq(
        new Operation(Instruction.INC_PTR, 1)
      )
    )
    assert(machine.tapePointer == 0)
  }
  test("Execute Decrement Pointer - Case 1:\nTape pointer at 0 should move to the last tape after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.DEC_PTR, 1)
      )
    )
    assert(machine.tapePointer == machine.tapes.length - 1)
  }
  test("Execute Decrement Pointer - Case 2:\nTape pointer at 0 should move to the second-to-last tape after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.DEC_PTR, 2)
      )
    )
    assert(machine.tapePointer == machine.tapes.length - 2)
  }
  test("Execute Decrement Pointer - Case 3:\nTape pointer at 1 should move to 0 after the execution.") {
    machine.tapePointer = 1
    machine.execute(
      Seq(
        new Operation(Instruction.DEC_PTR, 1)
      )
    )
    assert(machine.tapePointer == 0)
  }
  test("Execute Increment Value - Case 1:\nValue at the current tape (value of 0) should be 1 after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.INC_VAL, 1)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 1)
  }
  test("Execute Increment Value - Case 2:\nValue at the current tape (value of 0) should be 2 after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.INC_VAL, 2)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 2)
  }
  test("Execute Increment Value - Case 3:\nValue at the current tape (value of 127) should be 0 after the execution.") {
    machine.tapes(machine.tapePointer) = 127
    machine.execute(
      Seq(
        new Operation(Instruction.INC_VAL, 1)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 0)
  }
  test("Execute Decrement Value - Case 1:\nValue at the current tape (value of 0) should be 127 after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.DEC_VAL, 1)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 127)
  }
  test("Execute Decrement Value - Case 2:\nValue at the current tape (value of 0) should be 126 after the execution.") {
    machine.execute(
      Seq(
        new Operation(Instruction.DEC_VAL, 2)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 126)
  }
  test("Execute Decrement Value - Case 3:\nValue at the current tape (value of 1) should be 0 after the execution.") {
    machine.tapes(machine.tapePointer) = 1
    machine.execute(
      Seq(
        new Operation(Instruction.DEC_VAL, 1)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 0)
  }
  test("Execute Print to OutputStream - Case 1:\nShould print 1 'H' to OutputStream after the execution.") {
    machine.tapes(machine.tapePointer) = 72
    machine.execute(
      Seq(
        new Operation(Instruction.PRINT, 1)
      )
    )
    verify(out).print('H')
    verifyNoMoreInteractions(out)
  }
  test("Execute Print to OutputStream - Case 2:\nShould print 2 'H's to OutputStream after the execution.") {
    machine.tapes(machine.tapePointer) = 72
    machine.execute(
      Seq(
        new Operation(Instruction.PRINT, 2)
      )
    )
    verify(out, times(2)).print('H')
    verifyNoMoreInteractions(out)
  }
  test("Execute Read from InputStream - Case 1:\nShould read a value from InputStream and write 72 ('H') to the current tape after the execution.") {
    when(in.read)
      .thenReturn(72)
    machine.execute(
      Seq(
        new Operation(Instruction.READ, 1)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 72)
    verify(in).read
    verifyNoMoreInteractions(in)
  }
  test("Execute Read from InputStream - Case 2:\nShould read two values from InputStream and write 72 ('H') to the current tape after the execution.") {
    when(in.read)
      .thenReturn(71)
      .thenReturn(72)
    machine.execute(
      Seq(
        new Operation(Instruction.READ, 2)
      )
    )
    assert(machine.tapes(machine.tapePointer) == 72)
    verify(in, times(2)).read
    verifyNoMoreInteractions(in)
  }
  test("Execute 'Hello, World!' Program:\nShould print 'Hello, World!' to OutputStream.") {
    machine.execute(
      Seq(
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.OPEN_BRACKET, 12),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.DEC_PTR, 4),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 2),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.PRINT, 2),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 12),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_PTR, 2),
        new Operation(Instruction.INC_VAL, 15),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 6),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 8),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
    val printInOrder = inOrder(out)
    printInOrder.verify(out, times(1)).print('H')
    printInOrder.verify(out, times(1)).print('e')
    printInOrder.verify(out, times(2)).print('l')
    printInOrder.verify(out, times(1)).print('o')
    printInOrder.verify(out, times(1)).print(',')
    printInOrder.verify(out, times(1)).print(' ')
    printInOrder.verify(out, times(1)).print('W')
    printInOrder.verify(out, times(1)).print('o')
    printInOrder.verify(out, times(1)).print('r')
    printInOrder.verify(out, times(1)).print('l')
    printInOrder.verify(out, times(1)).print('d')
    printInOrder.verify(out, times(1)).print('!')
    printInOrder.verify(out, times(1)).print('\n')
    verifyNoMoreInteractions(out)
  }
}
