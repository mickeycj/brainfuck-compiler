package compiler
/** Imports */
import org.scalatest.FunSuite
/** CompilerSpec
 *
 *  Test suite for compiler functions.
 */
class CompilerSpec extends FunSuite {
  /** Tests for program tokenization.
   *
   *  Each sequence of tokens should contain only 8 valid BrainFuck's tokens: ">", "<", "+", "-", ".", ",", "[" and "]".
   *  Any other characters should be dropped during tokenization.
   */
  test("Tokenization - Case 1:\n>+<\nshould return the correct sequence of tokens.") {
    assert(
      Compiler.tokenize(">+<")
      ==
      Seq(
        '>', '+', '<'
      )
    )
  }
  test("Tokenization - Case 2:\n>,[>,]<[<]>[.>] # This acts like UNIX cat command\nshould return the correct sequence of tokens.") {
    assert(
      Compiler.tokenize(">,[>,]<[<]>[.>] # This acts like UNIX cat command")
      ==
      Seq(
        '>', ',',
        '[',
          '>', ',',
        ']',
        '<',
        '[',
          '<',
        ']',
        '>',
        '[',
          '.', '>',
        ']'
      )
    )
  }
  test("Tokenization - Case 3:\n++\n> +++++\n\n[\n\t< +\n\t> -\n]\nshould return the correct sequence of tokens.") {
    assert(
      Compiler.tokenize("++\n> +++++\n\n[\n\t< +\n\t> -\n]")
      ==
      Seq(
        '+', '+',
        '>', '+', '+', '+', '+', '+',
        '[',
          '<', '+',
          '>', '-',
        ']'
      )
    )
  }
  test("Tokenization - Case 4:\n+++++++\n++++ ++++\n[\n\t< +++ +++\n\t> -\n]\n< .\nshould return the correct sequence of tokens.") {
    assert(
      Compiler.tokenize("+++++++\n++++ ++++\n[\n\t< +++ +++\n\t> -\n]\n< .")
      ==
      Seq(
        '+', '+', '+', '+', '+', '+', '+',
        '+', '+', '+', '+', '+', '+', '+', '+',
        '[',
          '<', '+', '+', '+', '+', '+', '+',
          '>', '-',
        ']',
        '<', '.'
      )
    )
  }
  test("Tokenization - Case 5:\n+++++ +++++\n[\n\t> +++++ ++\n\t> +++++ +++++\n\t> +++\n\t> +\n\t<<<< -\n]\n> ++ .\n> + .\n+++++ ++ .\n.\n+++ .\n> ++ .\n<< +++++ +++++ +++++ .\n> .\n+++ .\n----- - .\n----- --- .\n> + .\n> .\nshould return the correct sequence of tokens.") {
    assert(
      Compiler.tokenize("+++++ +++++\n[\n\t> +++++ ++\n\t> +++++ +++++\n\t> +++\n\t> +\n\t<<<< -\n]\n> ++ .\n> + .\n+++++ ++ .\n.\n+++ .\n> ++ .\n<< +++++ +++++ +++++ .\n> .\n+++ .\n----- - .\n----- --- .\n> + .\n> .")
      ==
      Seq(
        '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
        '[',
          '>', '+', '+', '+', '+', '+', '+', '+',
          '>', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
          '>', '+', '+', '+',
          '>', '+',
          '<', '<', '<', '<', '-',
        ']',
        '>', '+', '+', '.',
        '>', '+', '.',
        '+', '+', '+', '+', '+', '+', '+', '.',
        '.',
        '+', '+', '+', '.',
        '>', '+', '+', '.',
        '<', '<', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '.',
        '>', '.',
        '+', '+', '+', '.',
        '-', '-', '-', '-', '-', '-', '.',
        '-', '-', '-', '-', '-', '-', '-', '-', '.',
        '>', '+', '.',
        '>', '.'
      )
    )
  }
  /** Tests for finding loop jumps.
   *
   *  Each open/closed bracket must have a matching pair.
   *  Else, the compilation fails with syntax error.
   */
  test("Find Closed Bracket - Case 1:\n>[+]<\nshould return the index of 3") {
    assert(
      Compiler.findMatchingClosedBracket(
        List('>', '[', '+', ']', '<'),
        1
      )
      ==
      3
    )
  }
  test("Find Closed Bracket - Case 2:\n>[+[--]>+]<\nshould return the index of 9 for the first open bracket") {
    assert(
      Compiler.findMatchingClosedBracket(
        List('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        1
      )
      ==
      9
    )
  }
  test("Find Closed Bracket - Case 3:\n>[+[--]>+]<\nshould return the index of 6 for the second open bracket") {
    assert(
      Compiler.findMatchingClosedBracket(
        List('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        3
      )
      ==
      6
    )
  }
  test("Find Closed Bracket - Case 4:\n>[+<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching closed bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingClosedBracket(
        List('>', '[', '+', '<'),
        1
      )
    }
    assert(e.getMessage == "Syntax Error: no matching closed bracket found!")
  }
  test("Find Closed Bracket - Case 5:\n>][+<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching closed bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingClosedBracket(
        List('>', ']', '[', '+', '<'),
        2
      )
    }
    assert(e.getMessage == "Syntax Error: no matching closed bracket found!")
  }
  test("Find Open Bracket - Case 1:\n>[+]<\nshould return the index of 1") {
    assert(
      Compiler.findMatchingOpenBracket(
        List('>', '[', '+', ']', '<'),
        3
      )
      ==
      1
    )
  }
  test("Find Open Bracket - Case 2:\n>[+[--]>+]<\nshould return the index of 1 for the last closed bracket") {
    assert(
      Compiler.findMatchingOpenBracket(
        List('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        9
      )
      ==
      1
    )
  }
  test("Find Open Bracket - Case 3:\n>[+[--]>+]<\nshould return the index of 3 for the second-to-last closed bracket") {
    assert(
      Compiler.findMatchingOpenBracket(
        List('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        6
      )
      ==
      3
    )
  }
  test("Find Open Bracket - Case 4:\n>+]<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingOpenBracket(
        List('>', '+', ']', '<'),
        2
      )
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  test("Find Open Bracket - Case 5:\n>+][<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingOpenBracket(
        List('>', '+', ']', '[', '<'),
        2
      )
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  /** Tests for operations mapping.
   *
   *  Each sequence of tokens should map to an appropriate sequence of operations with the right arguments.
   */
  test("Operations Mapping - Case 1:\n'>', '+' and '<'\nshould return the correct sequence of operations.") {
    assert(
      Compiler.mapToOperations(
        Seq(
          '>', '+', '<'
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.DEC_PTR, 1)
      )
    )
  }
  test("Operations Mapping - Case 2:\n'>', ',', '[', '>', ',', ']', '<', '[', '<', ']', '>', '[', '.', '>' and ']'\nshould return the correct sequence of operations.") {
    assert(
      Compiler.mapToOperations(
        Seq(
          '>', ',',
          '[',
            '>', ',',
          ']',
          '<',
          '[',
            '<',
          ']',
          '>',
          '[',
            '.', '>',
          ']'
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.READ, 1),
        new Operation(Instruction.OPEN_BRACKET, 5),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.READ, 1),
        new Operation(Instruction.CLOSED_BRACKET, 2),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.OPEN_BRACKET, 9),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.CLOSED_BRACKET, 7),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.OPEN_BRACKET, 14),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.CLOSED_BRACKET, 11)
      )
    )
  }
  test("Operations Mapping - Case 3:\n'+', '+', '>', '+', '+', '+', '+', '+', '[', '<', '+', '>', '-' and ']'\nshould return the correct sequence of operations.") {
    assert(
      Compiler.mapToOperations(
        Seq(
          '+', '+',
          '>', '+', '+', '+', '+', '+',
          '[',
            '<', '+',
            '>', '-',
          ']'
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.OPEN_BRACKET, 13),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 8)
      )
    )
  }
  test("Operations Mapping - Case 4:\n'+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '[', '<', '+', '+', '+', '+', '+', '+', '>', '-', ']', '<' and '.'\nshould return the correct sequence of operations.") {
    assert(
      Compiler.mapToOperations(
        Seq(
          '+', '+', '+', '+', '+', '+', '+',
          '+', '+', '+', '+', '+', '+', '+', '+',
          '[',
            '<', '+', '+', '+', '+', '+', '+',
            '>', '-',
          ']',
          '<', '.'
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.OPEN_BRACKET, 25),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 15),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
  }
  test("Operations Mapping - Case 5:\n'+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '[', '>', '+', '+', '+', '+', '+', '+', '+', '>', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '>', '+', '+', '+', '>', '+', '<', '<', '<', '<' '-', ']', '>', '+', '+', '.', '>', '+', '.', '+', '+', '+', '+', '+', '+', '+', '.', '.', '+', '+', '+', '.', '>', '+', '+', '.', '<', '<', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+' '.', '>', '.', '+', '+', '+', '.', '-', '-', '-', '-', '-', '-', '.', '-', '-', '-', '-', '-', '-', '-', '-', '.', '>', '+', '.', '>' and '.'\nshould return the correct sequence of operations.") {
    assert(
      Compiler.mapToOperations(
        Seq(
          '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
          '[',
            '>', '+', '+', '+', '+', '+', '+', '+',
            '>', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
            '>', '+', '+', '+',
            '>', '+',
            '<', '<', '<', '<', '-',
          ']',
          '>', '+', '+', '.',
          '>', '+', '.',
          '+', '+', '+', '+', '+', '+', '+', '.',
          '.',
          '+', '+', '+', '.',
          '>', '+', '+', '.',
          '<', '<', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '.',
          '>', '.',
          '+', '+', '+', '.',
          '-', '-', '-', '-', '-', '-', '.',
          '-', '-', '-', '-', '-', '-', '-', '-', '.',
          '>', '+', '.',
          '>', '.'
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.OPEN_BRACKET, 41),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 10),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
  }
}
