# Numerix Parser

Numerix is a programming language. This project provides a parser for the Numerix language, allowing you to translate Numerix code into C code.

## Prerequisites

You need to have Ocaml and GCC installed on your machine.

## Installation

Clone the repository to your local machine:

```bash
git clone https://github.com/Timeo1210/fast4numerix.git
```

Navigate to the project directory:

```bash
cd fast4numerix
```

Compile the parser:

```bash
ocamlc -o parseur lexeur.ml parseur.ml
```

## Usage

You can use the Numerix Parser to parse Numerix file. Here's an example:

```bash
./parser <input-file> <output-file>
```

This will parse the Numerix file in the input file and output the results.

After parsing, you can compile the output with GCC:

```bash
gcc -o example example.c
```
This will create an executable file named example.

## Example

This is an example of numerix language that compute 10!:

```ocaml
:= p 1;
:= i 1;
:= f 10;

:= n + f 1;
TantQue < i n Faire
  := p * p i;
  := i + i 1
FinTq
```

Find more in [`/examples`](https://github.com/Timeo1210/fast4numerix/blob/master/examples/)

## Credits

This project is based on the specifications provided in the [`Compilo_sujet.pdf`](https://github.com/Timeo1210/fast4numerix/blob/master/Compilo_sujet.pdf) document. We would like to express our gratitude to the authors of this document for providing the guidelines and requirements that shaped the development of this Numerix language parser.

## License

This project is licensed under the MIT License - see the [`LICENSE.md`](https://github.com/Timeo1210/fast4numerix/blob/master/LICENSE.md) file for details

## Links

Check me on [Twitter](https://twitter.com/TimeoBoulhol)