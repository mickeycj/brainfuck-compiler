package compiler
/** Brainfuck's set of instructions.
 *
 *  > : Increment the pointer
 *  < : Decrement the pointer
 *  + : Increment the value at the pointer
 *  - : Decrement the value at the pointer
 *  . : Print the value at the pointer to the console (ASCII characters)
 *  , : Read the value into the pointer (ASCII characters)
 *  [ : Jump to the matching closed bracket if the value at the pointer is 0
 *  ] : Jump to the matching open bracket if the value at the pointer is not 0 
 */
object Instruction {
  /** List of possible instructions. */
  val INC_PTR             = '>'
  val DEC_PTR             = '<'
  val INC_VAL             = '+'
  val DEC_VAL             = '-'
  val PRINT               = '.'
  val READ                = ','
  val OPEN_BRACKET        = '['
  val CLOSED_BRACKET      = ']'
  /** Regular expression for tokenization. */
  val INSTRUCTIONS_REGEX  = "[^><+\\-.,\\[\\]]"
}
