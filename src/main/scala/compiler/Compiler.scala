package compiler
/** Imports */
import scala.annotation.tailrec
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
   *  @param tokens the sequence of tokens
   *  @param openIndex the index of the current open bracket
   *  @return the index of the matching closed bracket
   */
  @throws(classOf[InvalidSyntaxException])
  def findMatchingClosedBracket(tokens: Seq[Char], openIndex: Int): Int = {
    findMatchingClosedBracketAccumulator(tokens, openIndex + 1)
  }
  /** Helper method for finding matching closed bracket.
   *
   *  @param tokens the sequence of tokens
   *  @param closedIndex the index of the possible closed bracket
   *  @param depth the current depth of the loop
   *  @return the index of the matching closed bracket
   */
  @tailrec
  @throws(classOf[InvalidSyntaxException])
  def findMatchingClosedBracketAccumulator(tokens: Seq[Char], closedIndex: Int, depth: Int = 1): Int = {
    depth match {
      case 0 => closedIndex - 1
      case _ => {
        val Length = tokens.length
        closedIndex match {
          case Length => throw new InvalidSyntaxException("Syntax Error: no matching closed bracket found!")
          case _ => {
            tokens(closedIndex) match {
              case Instruction.OPEN_BRACKET => findMatchingClosedBracketAccumulator(tokens, closedIndex + 1, depth + 1)
              case Instruction.CLOSED_BRACKET => findMatchingClosedBracketAccumulator(tokens, closedIndex + 1, depth - 1)
              case _ => findMatchingClosedBracketAccumulator(tokens, closedIndex + 1, depth)
            }
          }
        }
      }
    }
  }
  /** Find an open bracket that matches with the current closed bracket.
   *
   *  @param tokens the sequence of tokens
   *  @param closedIndex the index of the current closed bracket
   *  @return the index of the matching open bracket
   */
  @throws(classOf[InvalidSyntaxException])
  def findMatchingOpenBracket(tokens: Seq[Char], closedIndex: Int): Int = {
    findMatchingOpenBracketAccumulator(tokens, closedIndex - 1)
  }
  /** Helper method for finding matching open bracket.
   *
   *  @param tokens the sequence of tokens
   *  @param openIndex the index of the possible open bracket
   *  @param depth the current depth of the loop
   *  @return the index of the matching open bracket
   */
  @tailrec
  @throws(classOf[InvalidSyntaxException])
  def findMatchingOpenBracketAccumulator(tokens: Seq[Char], openIndex: Int, depth: Int = 1): Int = {
    depth match {
      case 0 => openIndex + 1
      case _ => {
        openIndex match {
          case -1 => throw new InvalidSyntaxException("Syntax Error: no matching open bracket found!")
          case _ => {
            tokens(openIndex) match {
              case Instruction.OPEN_BRACKET => findMatchingOpenBracketAccumulator(tokens, openIndex - 1, depth - 1)
              case Instruction.CLOSED_BRACKET => findMatchingOpenBracketAccumulator(tokens, openIndex - 1, depth + 1)
              case _ => findMatchingOpenBracketAccumulator(tokens, openIndex - 1, depth)
            }
          }
        }
      }
    }
  }
  /** Map each token to an appropriate operation.
   *
   *  @param tokens a sequence of tokens to be converted
   *  @return a sequence of operations
   */
  @throws(classOf[InvalidSyntaxException])
  def mapToOperations(tokens: Seq[Char]): Seq[Operation] = List[Operation]()
}
