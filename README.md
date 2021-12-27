# ocamlc-3 
<img src="https://img.aditya.diwakar.io/ocamlc3.png" height="128">

![OCaml Version](https://img.shields.io/badge/OCaml-v4.13.1-orange?style=for-the-badge)
![GitHub Workflow Status](https://img.shields.io/github/workflow/status/adityaxdiwakar/ocamlc-3/Build%20workflow?style=for-the-badge)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/adityaxdiwakar/ocamlc-3?style=for-the-badge)

OCamLC-3 is an LC-3 assembler written in OCaml (as the name suggests).

## LC-3
The LC-3 or Little Computer 3 is a fictitious computer intended to teach undergraduates about a datapath while building up from simple circuits to understanding assembly and C. The instruction set architecture is relatively simple and easy to understand and is detailed [here](https://github.com/adityaxdiwakar/ocamlc-3/blob/master/ISA.md).

## Interface (CLI)
The CLI is a work-in-progress, but will act as a way to lex, parse, or completely assemble an input `.asm` file. Examples of `asm` files are in the `tests/` directory.

## Assembly Process
The assembly process is relatively simple, as with any assembler. This is a regex-based assembler (unorthodox choice for lexing) that is comprised of 3 main steps: lexing, parsing, and assembling.

### Lexing
The lexing process takes an input file and reads line by line attempting to match the tokens by some priority. For example, before trying to match a label, it attempts to match an instruction (a label and instruction can both start with `AND`, hence the need for priority).

The lexer is detailed in `src/lexer.ml` built using a few productions that are matched with priorities. This returns a list of a list of lexed tokens (each list corresponding to each line).

### Parsing
The parsing process takes in a lexed file and parses it to ensure that instructions are properly informed. This is detailed in `src/parser.ml`, but essentially performs structural pattern matching on the line ensuring that instructions are followed by their neccesary requirements. For example, `AND` has to be followed with `2` registers and then either an additional register or a number (this number is bound checked to be `imm5` in the final assembly).

### Assembly
The assembly process is the final step converting parsed instructions into a binary file and is detailed in `src/assembler.ml`.
