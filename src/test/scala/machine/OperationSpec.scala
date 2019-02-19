package machine
/** Imports */
import org.scalatest.FunSuite
import compiler.Instruction
/** OperationSpec
 *
 *  Test suite for Operation class.
 */
class OperationSpec extends FunSuite {
  test("Reflexive Case:\nx.equals(x) should return 'true'") {
    val x = new Operation(Instruction.INC_PTR, 1)
    assert(x == x)
  }
  test("Symmetric Case:\nx.equals(y) should return 'true' if and only if y.equals(x) returns 'true'") {
    val x = new Operation(Instruction.INC_PTR, 1)
    val y = new Operation(Instruction.INC_PTR, 1)
    assert(x == y)
    assert(y == x)
  }
  test("Transitive Case:\nIf x.equals(y) returns 'true' and y.equals(z) returns 'true', then x.equals(z) should return 'true'") {
    val x = new Operation(Instruction.INC_PTR, 1)
    val y = new Operation(Instruction.INC_PTR, 1)
    val z = new Operation(Instruction.INC_PTR, 1)
    assert(x == y)
    assert(y == z)
    assert(x == z)
  }
  test("Inequality Case 1:\nDifferent operations should return 'false'") {
    val x = new Operation(Instruction.INC_PTR, 1)
    val y = new Operation(Instruction.DEC_PTR, 1)
    assert(x != y)
  }
  test("Inequality Case 2:\nSame operations with different argument should return 'false'") {
    val x = new Operation(Instruction.INC_PTR, 1)
    val y = new Operation(Instruction.INC_PTR, 2)
    assert(x != y)
  }
}
