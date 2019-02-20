package compiler
/** Imports */
import scala.annotation.tailrec
import machine.Operation
/** Object for compiling Brainfuck's codes.
 *
 *  This object contains functions that deal with tokenizing and compiling Brainfuck's codes into machine-readable operations.
 */
object Compiler {
  /** Tokenize the input program into valid Brainfuck's tokens.
   *
   *  @param program the string of the program to be tokenized
   *  @return a sequence of tokens
   */
  def tokenize(program: String): Seq[Char] = {
    program.replaceAll(Instruction.INSTRUCTIONS_REGEX, "").toArray.toList
  }
  /** Fold duplicated tokens into a single operation.
   *
   *  @param tokens a sequence of tokens to be converted
   *  @param position the current position of the token
   *  @return an operation with the appropriate argument
   */
  def foldTokens(tokens: Seq[Char], position: Int): Operation = {
    foldTokensAccumulator(tokens, position + 1)
  }
  /** Helper method for folding tokens.
   *
   *  @param tokens a sequence of tokens to be converted
   *  @param position the current position of the token
   *  @param count the number of duplicates found 
   *  @return an operation with theh appropriate argument
   */
  @tailrec
  def foldTokensAccumulator(tokens: Seq[Char], position: Int, count: Int = 1): Operation = {
    val PrevToken = tokens(position - 1)
    val Length = tokens.length
    position match {
      case Length => new Operation(PrevToken, count)
      case _ => {
        tokens(position) match {
          case PrevToken => foldTokensAccumulator(tokens, position + 1, count + 1)
          case _ => new Operation(PrevToken, count)
        }
      }
    }
  }
  /** Map each token to an appropriate operation.
   *
   *  @param tokens a sequence of tokens to be converted
   *  @return a sequence of operations
   */
  def mapToOperations(tokens: Seq[Char]): Seq[Operation] = {
    mapToOperationsAccumulator(tokens)
  }
  /** Helper method for mapping tokens to operations.
   *
   *  @param tokens a sequence of tokens to be converted
   *  @param position the current position of the token
   *  @param accum accumulated result
   *  @return a sequence of operations
   */
  @tailrec
  def mapToOperationsAccumulator(tokens: Seq[Char], position: Int = 0, accum: List[Operation] = List[Operation]()): Seq[Operation] = {
    val Length = tokens.length
    position match {
      case Length => accum.reverse
      case _ => {
        var operation = new Operation('\n', 0)
        var jump = 1
        val token = tokens(position)
        token match {
          case Instruction.OPEN_BRACKET | Instruction.CLOSED_BRACKET => operation = new Operation(token, accum.length)
          case _ => {
            operation = foldTokens(tokens, position)
            jump = operation.argument
          }
        }
        mapToOperationsAccumulator(tokens, position + jump, operation :: accum)
      }
    }
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
  /** Update the jumps in the specified sequence of operations.
   *
   *  @param operations the sequence of operations
   *  @return an updated sequence of operations
   */
  @throws(classOf[InvalidSyntaxException])
  def updateJumps(operations: Seq[Operation]): Seq[Operation] = {
    val tokens = operations.map(_.instruction)
    operations.map { case operation =>
      val token = operation.instruction
      val position = operation.argument
      token match {
        case Instruction.OPEN_BRACKET => new Operation(token, findMatchingClosedBracket(tokens, position))
        case Instruction.CLOSED_BRACKET => new Operation(token, findMatchingOpenBracket(tokens, position))
        case _ => operation
      }
    } 
  }
  /** Compile the input program into Brainfuck's operations.
   *
   *  @param program the string of the program to be compiled
   *  @return a sequence of operations
   */
  @throws(classOf[InvalidSyntaxException])
  def compile(program: String): Seq[Operation] = {
    updateJumps(mapToOperations(tokenize(program)))
  }
}
