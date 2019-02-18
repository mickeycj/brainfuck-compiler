package compiler
/** Operations for the machine to execute.
 *
 *  Each Operation instance contains the BrainFuck's instruction and the corresponding argument.
 */
class Operation(val instruction: String, val argument: Short) {
  /** Override 'hashCode' method. */
  override def hashCode: Int = {
    super.hashCode
  }
  /** Override 'equals' method. */
  override def equals(other: Any): Boolean = {
    super.equals(other)
  }
}
