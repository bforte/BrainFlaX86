# BrainFlaX86

```
usage: brainflax86 [-s | -x] [-e SRC | SRC_FILE] [-o OUT_FILE]

  -e EXPR  --expr=EXPR  specify source
  -o FILE  --out=FILE   output file
  -s       --asm        only generate asm code
  -x       --exec       compile & run the source
```


## Implementation

This implementation of [Brain-Flak](https://github.com/DJMcMayhem/Brain-Flak)
compiles to x86 machine code. The binary takes the `-a` flag to switch to ASCII
mode, however it doesn't support ASCII inputs. Another difference to the
original implementation is that it uses 32bit Two's complement numbers, meaning
that there's a possibility for overflows:

```bash
$ brainflax86 -xe "({}{})" 2147483647 1
-2147483648
```

## Usage examples


Compiling a program (without `-o` flag it defaults to `bf.out`):

```bash
$ brainflax86 -e source.flk -o my_program
```

Generating assembly code and writing it to *stdout* (output not shown):

```bash
$ brainflax86 -s source.flk -o -
```

Running source directly:

```bash
$ brainflax86 -xe "({{}})" 1 2 3 4
10
```

Running source directly and supplying the `-a` flag:

```bash
$ brainflax86 -xe "comment" -- -a 120 56 54 10
x86
```

<sub>Note how it treats every character other than `()[]{}<>` as comments.</sub>


## Building

If you only want to build the compiler, one of the following should do the
trick:

  - `ghc src/*.hs -o brainflax86`
  - `cabal build`
  - `stack setup && stack build`


## Installing


If you used the first option to build, simply move the binary to a directory in
your `PATH`. Otherwise you can install it with (probably you'll need to add
`~/.local/bin` to your `PATH`):

  - `cabal install`
  - `stack setup && stack install`
