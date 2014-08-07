# haskell-brainfuck

Interpreter for the
[brainfuck programming language](http://www.muppetlabs.com/~breadbox/bf/)

[![Build Status](https://api.travis-ci.org/paraseba/haskell-brainfuck.svg?branch=master)](https://travis-ci.org/paraseba/haskell-brainfuck)

haskel-brainfuck is distributed as a library, but it also includes an executable
to run brainfuck programs.

## Usage
### Library
```haskell
import HaskBF.Eval

-- Will result in a EvalResult
evalBS defaultIOMachine
```

### Executable
```bash
brainfuck fib.bf
```

