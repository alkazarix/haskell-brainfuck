# haskell-brainfuck

Interpreter for the [brainfuck programming language](https://fr.wikipedia.org/wiki/Brainfuck)


### Usage

```haskell
import BrainFuck.Eval as BF
import qualified Data.ByteString.Lazy as BS
main = do

  content <- BS.readFile "/path/to/your/file.bf"
  result <- BF.evalString content

  case result of 
    Left bfError -> print bfError
    Right bfMachine -> print "ok"
```

