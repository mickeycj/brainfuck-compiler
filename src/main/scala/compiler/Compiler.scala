package compiler
/** Object for compiling BrainFuck's codes.
 *
 *  This object contains functions that deal with tokenizing and compiling BrainFuck's codes into machine-readable operations.
 */
object Compiler extends App {
  /** Tokenize the input program into valid BrainFuck's tokens.
   *
   *  @param program the string of the program to be tokenized
   *  @return a sequence of tokens
   */
  def tokenize(program: String): Seq[Char] = {
    program.replaceAll(Instruction.INSTRUCTIONS_REGEX, "").toArray.toList
  }
  /** Find a closed bracket that matches with the current open bracket.
   *
   *  @param operations the sequence of operations 
   *  @param openIndex the index of the current open bracket
   *  @return the index of the matching closed bracket
   */
  @throws(classOf[InvalidSyntaxException])
  def findMatchingClosedBracket(operations: Seq[Char], openIndex: Short): Short = -1
  /** Find an open bracket that matches with the current closed bracket.
   *
   *  @param operations the sequence of operations 
   *  @param closedIndex the index of the current closed bracket
   *  @return the index of the matching open bracket
   */
  @throws(classOf[InvalidSyntaxException])
  def findMatchingOpenBracket(operations: Seq[Char], closedIndex: Short): Short = -1
}
