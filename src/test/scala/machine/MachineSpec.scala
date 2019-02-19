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
  /** Test if Mockito works.
   *
   *  This test simply tests whether the Machine instance is instantiated at all.
   */
  test("Test") {
    assert(machine.tapes.length == machine.tapes.length)
  }
}
