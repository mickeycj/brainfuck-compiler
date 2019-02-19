package machine
/** Operations for the machine to execute.
 *
 *  Each Operation instance contains the BrainFuck's instruction and the corresponding argument.
 */
class Operation(val instruction: Char, val argument: Int) {
  /** Override 'hashCode' method. */
  override def hashCode: Int = {
    var result = 1
    result = 31 * result + instruction.hashCode
    result = 31 * result + argument.hashCode
    result
  }
  /** Override 'equals' method. */
  override def equals(other: Any): Boolean = {
    other match {
      case other: Operation => other.isInstanceOf[Operation] && this.hashCode == other.hashCode
      case _ => false
    }
  }
  /** Override 'toString' method. */
  override def toString: String = {
    s"Operation: { instruction: $instruction, argument: $argument }"
  }
}
