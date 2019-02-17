package compiler
/** BrainFuck's set of instructions.
 *
 *  ">" : Increment the pointer
 *  "<" : Decrement the pointer
 *  "+" : Increment the value at the pointer
 *  "-" : Decrement the value at the pointer
 *  "." : Print the value at the pointer to the console (ASCII characters)
 *  "," : Read the value into the pointer (ASCII characters)
 *  "[" : Jump to the matching closed bracket if the value at the pointer is 0
 *  "]" : Jump to the matching open bracket if the value at the pointer is not 0 
 */
object Instruction extends Enumeration {
  type Instruction    = Value
  val INC_PTR         = Value(">")
  val DEC_PTR         = Value("<")
  val INC_VAL         = Value("+")
  val DEC_VAL         = Value("-")
  val PRINT           = Value(".")
  val READ            = Value(",")
  val OPEN_BRACKET    = Value("[")
  val CLOSED_BRACKET  = Value("]")
}
