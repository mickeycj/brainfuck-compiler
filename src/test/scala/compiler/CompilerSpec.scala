package compiler
/** Imports */
import org.scalatest.FunSuite
import machine.Operation
/** CompilerSpec
 *
 *  Test suite for compiler functions.
 */
class CompilerSpec extends FunSuite {
  /** Tests for program tokenization.
   *
   *  Each sequence of tokens should contain only 8 valid Brainfuck's tokens: ">", "<", "+", "-", ".", ",", "[" and "]".
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
  test("Tokenization - Case 5:\n+++++ +++++\n[\n\t> +++++ ++\n\t> +++++ +++++\n\t> ++++\n\t> +\n\t<<<< -\n]\n> ++ .\n> + .\n+++++ ++ .\n.\n+++ .\n> ++++ .\n----- ----- -- .\n<< +++++ +++++ +++++ .\n> .\n+++ .\n----- - .\n----- --- .\n> + .\n> .\nshould return the correct sequence of tokens.") {
    assert(
      Compiler.tokenize("+++++ +++++\n[\n\t> +++++ ++\n\t> +++++ +++++\n\t> ++++\n\t> +\n\t<<<< -\n]\n> ++ .\n> + .\n+++++ ++ .\n.\n+++ .\n> ++++ .\n----- ----- -- .\n<< +++++ +++++ +++++ .\n> .\n+++ .\n----- - .\n----- --- .\n> + .\n> .")
      ==
      Seq(
        '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
        '[',
          '>', '+', '+', '+', '+', '+', '+', '+',
          '>', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
          '>', '+', '+', '+', '+',
          '>', '+',
          '<', '<', '<', '<', '-',
        ']',
        '>', '+', '+', '.',
        '>', '+', '.',
        '+', '+', '+', '+', '+', '+', '+', '.',
        '.',
        '+', '+', '+', '.',
        '>', '+', '+', '+', '+', '.',
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '.',
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
  /** Tests for tokens folding.
   *
   *  Each sequence of duplicated tokens should fold into a single operation with the appropriate argument.
   */
  test("Tokens Folding - Case 1:\nshould return an operation with 1 as an argument") {
    assert(
      Compiler.foldTokens(
        Seq(
          '>', '+', '<'
        ),
        1
      )
      ==
      new Operation(Instruction.INC_VAL, 1)
    )
  }
  test("Tokens Folding - Case 2:\nshould return an operation with 2 as an argument") {
    assert(
      Compiler.foldTokens(
        Seq(
          '+', '+',
          '>', '+', '+', '+', '+', '+',
          '[',
            '<', '+',
            '>', '-',
          ']'
        ),
        0
      )
      ==
      new Operation(Instruction.INC_VAL, 2)
    )
  }
  test("Tokens Folding - Case 3:\nshould return an operation with 15 as an argument") {
    assert(
      Compiler.foldTokens(
        Seq(
          '+', '+', '+', '+', '+', '+', '+',
          '+', '+', '+', '+', '+', '+', '+', '+',
          '[',
            '<', '+', '+', '+', '+', '+', '+',
            '>', '-',
          ']',
          '<', '.'
        ),
        0
      )
      ==
      new Operation(Instruction.INC_VAL, 15)
    )
  }
  /** Tests for initial operations mapping.
   *
   *  Each sequence of tokens should map to an appropriate sequence of operations.
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
        new Operation(Instruction.OPEN_BRACKET, 2),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.READ, 1),
        new Operation(Instruction.CLOSED_BRACKET, 5),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.OPEN_BRACKET, 7),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.CLOSED_BRACKET, 9),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.OPEN_BRACKET, 11),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.CLOSED_BRACKET, 14)
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
        new Operation(Instruction.INC_VAL, 2),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 5),
        new Operation(Instruction.OPEN_BRACKET, 3),
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
        new Operation(Instruction.INC_VAL, 15),
        new Operation(Instruction.OPEN_BRACKET, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.INC_VAL, 6),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 6),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
  }
  test("Operations Mapping - Case 5:\n'+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '[', '>', '+', '+', '+', '+', '+', '+', '+', '>', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '>', '+', '+', '+', '+', '>', '+', '<', '<', '<', '<', '-', ']', '>', '+', '+', '.', '>', '+', '.', '+', '+', '+', '+', '+', '+', '+', '.', '.', '+', '+', '+', '.', '>', '+', '+', '+', '+', '.', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '.', '<', '<', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '.', '>', '.', '+', '+', '+', '.', '-', '-', '-', '-', '-', '-', '.', '-', '-', '-', '-', '-', '-', '-', '-', '.', '>', '+', '.', '>' and '.'\nshould return the correct sequence of operations.") {
    assert(
      Compiler.mapToOperations(
        Seq(
          '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
          '[',
            '>', '+', '+', '+', '+', '+', '+', '+',
            '>', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+',
            '>', '+', '+', '+', '+',
            '>', '+',
            '<', '<', '<', '<', '-',
          ']',
          '>', '+', '+', '.',
          '>', '+', '.',
          '+', '+', '+', '+', '+', '+', '+', '.',
          '.',
          '+', '+', '+', '.',
          '>', '+', '+', '+', '+', '.',
          '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '.',
          '<', '<', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '+', '.',
          '>', '.',
          '+', '+', '+', '.',
          '-', '-', '-', '-', '-', '-', '.',
          '-', '-', '-', '-', '-', '-', '-', '-', '.',
          '>', '+', '.',
          '>', '.'
        )
      )
      ===
      Seq(
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.OPEN_BRACKET, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.DEC_PTR, 4),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 12),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 2),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.PRINT, 2),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 12),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_PTR, 2),
        new Operation(Instruction.INC_VAL, 15),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 6),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 8),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
  }
  /** Tests for finding loop jumps.
   *
   *  Each open/closed bracket must have a matching pair.
   *  Else, the compilation fails with syntax error.
   */
  test("Find Closed Bracket - Case 1:\n'>', '[', '+', ']' and '<'\nshould return the index of 3") {
    assert(
      Compiler.findMatchingClosedBracket(
        Seq('>', '[', '+', ']', '<'),
        1
      )
      ==
      3
    )
  }
  test("Find Closed Bracket - Case 2:\n'>', '[', '+', '[', '-', '-', ']', '>', '+', ']' and '<'\nshould return the index of 9 for the first open bracket") {
    assert(
      Compiler.findMatchingClosedBracket(
        Seq('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        1
      )
      ==
      9
    )
  }
  test("Find Closed Bracket - Case 3:\n'>', '[', '+', '[', '-', '-', ']', '>', '+', ']' and '<'\nshould return the index of 6 for the second open bracket") {
    assert(
      Compiler.findMatchingClosedBracket(
        Seq('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        3
      )
      ==
      6
    )
  }
  test("Find Closed Bracket - Case 4:\n'>', '[', '+' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching closed bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingClosedBracket(
        Seq('>', '[', '+', '<'),
        1
      )
    }
    assert(e.getMessage == "Syntax Error: no matching closed bracket found!")
  }
  test("Find Closed Bracket - Case 5:\n'>', ']', '[', '+' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching closed bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingClosedBracket(
        Seq('>', ']', '[', '+', '<'),
        2
      )
    }
    assert(e.getMessage == "Syntax Error: no matching closed bracket found!")
  }
  test("Find Open Bracket - Case 1:\n'>', '[', '+', ']' and '<'\nshould return the index of 1") {
    assert(
      Compiler.findMatchingOpenBracket(
        Seq('>', '[', '+', ']', '<'),
        3
      )
      ==
      1
    )
  }
  test("Find Open Bracket - Case 2:\n'>', '[', '+', '[', '-', '-', ']', '>', '+', ']' and '<'\nshould return the index of 1 for the last closed bracket") {
    assert(
      Compiler.findMatchingOpenBracket(
        Seq('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        9
      )
      ==
      1
    )
  }
  test("Find Open Bracket - Case 3:\n'>', '[', '+', '[', '-', '-', ']', '>', '+', ']' and '<'\nshould return the index of 3 for the second-to-last closed bracket") {
    assert(
      Compiler.findMatchingOpenBracket(
        Seq('>', '[', '+', '[', '-', '-', ']', '>', '+', ']', '<'),
        6
      )
      ==
      3
    )
  }
  test("Find Open Bracket - Case 4:\n'>', '+', ']' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingOpenBracket(
        
        Seq('>', '+', ']', '<'),
        2
      )
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  test("Find Open Bracket - Case 5:\n'>', '+', ']', '[' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.findMatchingOpenBracket(
        Seq('>', '+', ']', '[', '<'),
        2
      )
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  test("Jumps Update - Case 1:\n'>', '[', '+', ']' and '<'\nshould correctly update the jumps.") {
    assert(
      Compiler.updateJumps(
        Seq(
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.OPEN_BRACKET, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.CLOSED_BRACKET, 3),
          new Operation(Instruction.DEC_PTR, 1)
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.OPEN_BRACKET, 3),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 1),
        new Operation(Instruction.DEC_PTR, 1)
      )
    )
  }
  test("Jumps Update - Case 2:\n'+', '>', '+', '[', '<', '+', '>', '-' and ']'\nshould correctly update the jumps.") {
    assert(
      Compiler.updateJumps(
        Seq(
          new Operation(Instruction.INC_VAL, 2),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 5),
          new Operation(Instruction.OPEN_BRACKET, 3),
          new Operation(Instruction.DEC_PTR, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.DEC_VAL, 1),
          new Operation(Instruction.CLOSED_BRACKET, 8)
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_VAL, 2),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 5),
        new Operation(Instruction.OPEN_BRACKET, 8),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 3)
      )
    )
  }
  test("Jumps Update - Case 3:\n'+', '[', '>', '+', '>', '+', '>', '+', '>', '+', '<', '-', ']', '>', '+', '.', '>', '+', '.', '+', '.', '+', '.', '>', '+', '.', '-', '.', '<', '+', '.', '>', '.', '+', '.', '-', '.', '-', '.', '>', '+', '.', '>' and '.'\nshould correctly update teh jumps.") {
    assert(
      Compiler.updateJumps(
        Seq(
          new Operation(Instruction.INC_VAL, 10),
          new Operation(Instruction.OPEN_BRACKET, 1),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 7),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 10),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 4),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.DEC_PTR, 4),
          new Operation(Instruction.DEC_VAL, 1),
          new Operation(Instruction.CLOSED_BRACKET, 12),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 2),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.INC_VAL, 7),
          new Operation(Instruction.PRINT, 2),
          new Operation(Instruction.INC_VAL, 3),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 4),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.DEC_VAL, 12),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.DEC_PTR, 2),
          new Operation(Instruction.INC_VAL, 15),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.INC_VAL, 3),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.DEC_VAL, 6),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.DEC_VAL, 8),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.PRINT, 1),
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.PRINT, 1)
        )
      )
      ==
      Seq(
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.OPEN_BRACKET, 12),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.DEC_PTR, 4),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 2),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.PRINT, 2),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 12),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_PTR, 2),
        new Operation(Instruction.INC_VAL, 15),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 6),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 8),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
  }
  test("Jumps Update - Case 4:\n'>', '[', '+' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching closed bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.updateJumps(
        Seq(
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.OPEN_BRACKET, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.DEC_PTR, 1)
        )
      )
    }
    assert(e.getMessage == "Syntax Error: no matching closed bracket found!")
  }
  test("Jumps Update - Case 5:\n'>', ']', '[', '+' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching closed bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.updateJumps(
        Seq(
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.CLOSED_BRACKET, 1),
          new Operation(Instruction.OPEN_BRACKET, 2),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.DEC_PTR, 1)
        )
      )
    }
    assert(e.getMessage == "Syntax Error: no matching closed bracket found!")
  }
  test("Jumps Update - Case 6:\n'>', '+', ']' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.updateJumps(
        Seq(
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.CLOSED_BRACKET, 2),
          new Operation(Instruction.DEC_PTR, 1)
        )
      )
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  test("Jumps Update - Case 7:\n'>', '+', ']', '[' and '<'\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.updateJumps(
        Seq(
          new Operation(Instruction.INC_PTR, 1),
          new Operation(Instruction.INC_VAL, 1),
          new Operation(Instruction.CLOSED_BRACKET, 2),
          new Operation(Instruction.OPEN_BRACKET, 3),
          new Operation(Instruction.DEC_PTR, 1)
        )
      )
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  /** Tests for program compilation.
   *
   *  Each input program should produce an appropriate sequence of operations.
   */
  test("Program Compilation - Case 1:\n>+<\nshould return the correct sequence of operations.") {
    assert(
      Compiler.compile(">+<")
      ==
      Seq(
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.DEC_PTR, 1)
      )
    )
  }
  test("Program Compilation - Case 2:\n>,[>,]<[<]>[.>] # This acts like UNIX cat command\nshould return the correct sequence of operations.") {
    assert(
      Compiler.compile(">,[>,]<[<]>[.>] # This acts like UNIX cat command")
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
  test("Program Compilation - Case 3:\n++\n> +++++\n\n[\n\t< +\n\t> -\n]\nshould return the correct sequence of operations.") {
    assert(
      Compiler.compile("++\n> +++++\n\n[\n\t< +\n\t> -\n]")
      ==
      Seq(
        new Operation(Instruction.INC_VAL, 2),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 5),
        new Operation(Instruction.OPEN_BRACKET, 8),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 3)
      )
    )
  }
  test("Program Compilation - Case 4:\n+++++++\n++++ ++++\n[\n\t< +++ +++\n\t> -\n]\n< .\nshould return the correct sequence of operations.") {
    assert(
      Compiler.compile("+++++++\n++++ ++++\n[\n\t< +++ +++\n\t> -\n]\n< .")
      ==
      Seq(
        new Operation(Instruction.INC_VAL, 15),
        new Operation(Instruction.OPEN_BRACKET, 6),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.INC_VAL, 6),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 1),
        new Operation(Instruction.DEC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
  }
  test("Program Compilation - Case 5:\n+++++ +++++\n[\n\t> +++++ ++\n\t> +++++ +++++\n\t> ++++\n\t> +\n\t<<<< -\n]\n> ++ .\n> + .\n+++++ ++ .\n.\n+++ .\n> ++++ .\n----- ----- -- .\n<< +++++ +++++ +++++ .\n> .\n+++ .\n----- - .\n----- --- .\n> + .\n> .\nshould return the correct sequence of operations.") {
    assert(
      Compiler.compile("+++++ +++++\n[\n\t> +++++ ++\n\t> +++++ +++++\n\t> ++++\n\t> +\n\t<<<< -\n]\n> ++ .\n> + .\n+++++ ++ .\n.\n+++ .\n> ++++ .\n----- ----- -- .\n<< +++++ +++++ +++++ .\n> .\n+++ .\n----- - .\n----- --- .\n> + .\n> .")
      ===
      Seq(
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.OPEN_BRACKET, 12),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 10),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.DEC_PTR, 4),
        new Operation(Instruction.DEC_VAL, 1),
        new Operation(Instruction.CLOSED_BRACKET, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 2),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 7),
        new Operation(Instruction.PRINT, 2),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 4),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 12),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_PTR, 2),
        new Operation(Instruction.INC_VAL, 15),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_VAL, 3),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 6),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.DEC_VAL, 8),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.INC_VAL, 1),
        new Operation(Instruction.PRINT, 1),
        new Operation(Instruction.INC_PTR, 1),
        new Operation(Instruction.PRINT, 1)
      )
    )
  }
  test("Program Compilation - Case 6:\n>[+<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching closed bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.compile(">[+<")
    }
    assert(e.getMessage == "Syntax Error: no matching closed bracket found!")
  }
  test("Program Compilation - Case 7:\n>][+<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.compile(">][+<")
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  test("Program Compilation - Case 8:\n>+]<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.compile(">+]<")
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
  test("Program Compilation - Case 9:\n>+][<\nshould throw InvalidSyntaxException with message: \"Syntax Error: no matching open bracket found!\"") {
    val e = intercept[InvalidSyntaxException] {
      Compiler.compile(">+][<")
    }
    assert(e.getMessage == "Syntax Error: no matching open bracket found!")
  }
}
