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
   *  @param operations the sequence of operations
   *  @param openIndex the index of the current open bracket
   *  @return the index of the matching closed bracket
   */
  @throws(classOf[InvalidSyntaxException])
  def findMatchingClosedBracket(operations: Seq[Char], openIndex: Int): Int = {
    findMatchingClosedBracketAccumulator(operations, openIndex + 1)
  }
  /** Helper method for finding matching closed bracket.
   *
   *  @param operations the sequence of operations
   *  @param closedIndex the index of the possible closed bracket
   *  @param depth the current depth of the loop
   *  @return the index of the matching closed bracket
   */
  @tailrec
  @throws(classOf[InvalidSyntaxException])
  def findMatchingClosedBracketAccumulator(operations: Seq[Char], closedIndex: Int, depth: Int = 1): Int = {
    depth match {
      case 0 => closedIndex - 1
      case _ => {
        val Length = operations.length
        closedIndex match {
          case Length => throw new InvalidSyntaxException("Syntax Error: no matching closed bracket found!")
          case _ => {
            operations(closedIndex) match {
              case Instruction.OPEN_BRACKET => findMatchingClosedBracketAccumulator(operations, closedIndex + 1, depth + 1)
              case Instruction.CLOSED_BRACKET => findMatchingClosedBracketAccumulator(operations, closedIndex + 1, depth - 1)
              case _ => findMatchingClosedBracketAccumulator(operations, closedIndex + 1, depth)
            }
          }
        }
      }
    }
  }
  /** Find an open bracket that matches with the current closed bracket.
   *
   *  @param operations the sequence of operations
   *  @param closedIndex the index of the current closed bracket
   *  @return the index of the matching open bracket
   */
  @throws(classOf[InvalidSyntaxException])
  def findMatchingOpenBracket(operations: Seq[Char], closedIndex: Int): Int = {
    findMatchingOpenBracketAccumulator(operations, closedIndex - 1)
  }
  /** Helper method for finding matching open bracket.
   *
   *  @param operations the sequence of operations
   *  @param openIndex the index of the possible open bracket
   *  @param depth the current depth of the loop
   *  @return the index of the matching open bracket
   */
  @tailrec
  @throws(classOf[InvalidSyntaxException])
  def findMatchingOpenBracketAccumulator(operations: Seq[Char], openIndex: Int, depth: Int = 1): Int = {
    depth match {
      case 0 => openIndex + 1
      case _ => {
        openIndex match {
          case -1 => throw new InvalidSyntaxException("Syntax Error: no matching open bracket found!")
          case _ => {
            operations(openIndex) match {
              case Instruction.OPEN_BRACKET => findMatchingOpenBracketAccumulator(operations, openIndex - 1, depth - 1)
              case Instruction.CLOSED_BRACKET => findMatchingOpenBracketAccumulator(operations, openIndex - 1, depth + 1)
              case _ => findMatchingOpenBracketAccumulator(operations, openIndex - 1, depth)
            }
          }
        }
      }
    }
  }
}
