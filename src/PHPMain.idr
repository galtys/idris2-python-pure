module PHPMain

import Core.Context
import Compiler.Common
import Idris.Driver
import Idris.Syntax
import Idris.Env
import Core.Options
import Libraries.Utils.Path
import PHP.Codegen as PHPCg

%default covering

compileToPhp : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> ClosedTerm -> Core String
compileToPhp c s tm = PHPCg.compileToES c s Node tm ["php8"]

compileExpr :  Ref Ctxt Defs
            -> Ref Syn SyntaxInfo
            -> (tmpDir : String)
            -> (outputDir : String)
            -> ClosedTerm
            -> (outfile : String)
            -> Core (Maybe String)
compileExpr c syn tmpDir outputDir tm outfile = do
  php <- compileToPhp c syn tm
  let out = outputDir </> outfile
  Core.writeFile out php
  pure (Just out)

executeE : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> (execDir : String) -> ClosedTerm -> Core ()
executeE defs syn dir term = coreLift $ putStrLn "PHP execution not supported; compile and run with php."

lazyCodegen : Codegen
lazyCodegen = MkCG compileExpr executeE Nothing Nothing

main : IO ()
main = mainWithCodegens [("php8", lazyCodegen)]
