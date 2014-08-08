# haskell-brainfuck

Interpreter for the
[brainfuck programming language](http://www.muppetlabs.com/~breadbox/bf/)

[![Build Status](https://api.travis-ci.org/paraseba/haskell-brainfuck.svg?branch=master)](https://travis-ci.org/paraseba/haskell-brainfuck)

haskel-brainfuck is distributed as a library, but it also includes an executable
to run brainfuck programs. You can find haskell-brainfuck in
[Hackage](https://hackage.haskell.org/package/haskell-brainfuck-0.1.0.0)

## Usage
### Library
```haskell
import HaskBF.Eval
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State

main = do
  -- The following will evaluate the file using stdin and stdout for I/O.
  -- Evaluation results in an EvalResult

  file <- BS.readFile "/path/to/file.bf"
  (EvalSuccess _) <- evalBS defaultIOMachine file
  print "ok"


  -- The following will evaluate the file using the State monad and input
  -- provided by input

  let input  = []
      output = []
      result = execState (evalStr simulatorMachine "+.>-.") (SimState input output)
  print $ simStateOutput result == [1, -1]
```

### Executable
```bash
brainfuck fib.bf
```

## Documentation
http://paraseba.github.io/haskell-brainfuck/
