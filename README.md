# Brainfuck Compiler

**Brainfuck** is a Turing complete, esoteric programming language created in 1993 by Urban Müller. It consists of only eight commands and an instruction pointer. You can read more about it [here](https://en.wikipedia.org/wiki/Brainfuck).

Below is an example of a 'Hello, World!' program in Brainfuck:

```brainfuck
++++++++++[>+++++++>++++++++++>++++>+<<<<-]>++.>+.+++++++..+++.>++++.------------.<<+++++++++++++++.>.+++.------.--------.>+.>.
```

## Compiler

A Brainfuck compiler must operate on only these eight commands below. Any other characters should be ignored by the compiler.

```
> : Increment the pointer
< : Decrement the pointer
+ : Increment the value at the pointer
- : Decrement the value at the pointer
. : Print the value at the pointer to the console (ASCII characters)
, : Read the value into the pointer (ASCII characters)
[ : Jump to the matching closed bracket if the value at the pointer is 0
] : Jump to the matching open bracket if the value at the pointer is not 0
```

Moreover, in this implementation of the compiler, any open bracket (`[`) that does not have a matching closed bracket (`]`) will cause the code to not compile and end with a syntax error. In other implementations, they will try to find the matching bracket in reverse, but will affect the performance.

## Installation / Usage

First, follow `sbt` installation steps [here](https://www.scala-sbt.org/1.0/docs/Setup.html).

Then, after cloning this repository, run the main app using this command:

```sh
sbt run
```

You should see the result of your Brainfuck's program like this:

```sh
.
.
[info] Running Main

Hello, World!

[success] Total time: 1 s, completed Jan 1, 2019 12:00:00 AM
.
.
```

In case you would like to run another Brainfuck program, you can put it in the `./src/main/resources` folder and edit the Brainfuck's file name in `./src/main/scala/Main.scala`
