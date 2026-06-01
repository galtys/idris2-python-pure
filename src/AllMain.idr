module AllMain

import Core.Context
import Compiler.Common
import Idris.Driver
import Idris.Syntax
import Idris.Env
import Core.Options
import Libraries.Utils.Path
import Py.Codegen as PyCg
import PHP.Codegen as PHPCg

%default covering

compilePy : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> ClosedTerm -> Core String
compilePy c s tm = PyCg.compileToES c s Node tm ["pygen"]

compilePhp : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> ClosedTerm -> Core String
compilePhp c s tm = PHPCg.compileToES c s Node tm ["php8"]

mkCodegen : (Ref Ctxt Defs -> Ref Syn SyntaxInfo -> ClosedTerm -> Core String) -> Codegen
mkCodegen compile = MkCG compileExpr executeE Nothing Nothing
  where
    compileExpr :  Ref Ctxt Defs
                -> Ref Syn SyntaxInfo
                -> (tmpDir : String)
                -> (outputDir : String)
                -> ClosedTerm
                -> (outfile : String)
                -> Core (Maybe String)
    compileExpr c syn tmpDir outputDir tm outfile = do
      src <- compile c syn tm
      let out = outputDir </> outfile
      Core.writeFile out src
      pure (Just out)

    executeE : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> (execDir : String) -> ClosedTerm -> Core ()
    executeE _ _ _ _ = coreLift $ putStrLn "Direct execution not supported; compile and run with the target runtime."

main : IO ()
main = mainWithCodegens [("pygen", mkCodegen compilePy), ("php8", mkCodegen compilePhp)]
