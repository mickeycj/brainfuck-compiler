package machine
/** Operations for the machine to execute.
 *
 *  Each Operation instance contains the Brainfuck's instruction and the corresponding argument.
 */
case class Operation(instruction: Char, argument: Int) {
  /** Override 'toString' method */
  override def toString: String = s"Operation: { instruction: $instruction, argument: $argument }"
}
