module Main

import Compiler
import Control.Monad.Either
import Control.Monad.State
import Core
import Elf

main : IO ()
main = case runCompile _main of
  Right (codeBs, dataBs) => do
    writeProgram "/tmp/hello2" $ program codeBs dataBs
    pure ()
  Left (NoFunction e) => putStrLn ("NoFunction " ++ e)
  Left _ => putStrLn "Unknown CompilerError"
