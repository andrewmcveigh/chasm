module Main

import Compiler
import Elf

main : IO ()
main =
  let (codeBs, dataBs) = runCompile helloWorld
  in do writeProgram "/tmp/hello2" $ program codeBs dataBs
        pure ()
