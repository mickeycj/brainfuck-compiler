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
    val regex = "[^" + Instruction.values.toList.foldLeft("") { (acc, i) => acc + "\\" + i } + "]"
    program.replaceAll(regex, "").toArray.toList
  }
}
