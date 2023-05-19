module PyMain

import Core.Context
import Compiler.Common
import Idris.Driver
import Idris.Syntax
import Idris.Env
--import Compiler.ES.Codegen

import Core.Options
--import Core.TT
import Libraries.Utils.Path
import Py.Codegen as PyCg

--import System

--import Data.Maybe

%default covering

kocour : Bool -> Int

{-
compile : Ref Ctxt Defs -> (tmpDir : String) -> (execDir : String) ->
        ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile defs tmp dir term file = do coreLift $ putStrLn "I'd rather not."
                                    pure $ Nothing

-}
||| Compile a TT expression to Node
compileToNode : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> ClosedTerm -> Core String
compileToNode c s tm = PyCg.compileToES c s Node tm ["pygen"] --["node", "javascript"]

||| Node implementation of the `compileExpr` interface.
compileExpr :  Ref Ctxt Defs
            -> Ref Syn SyntaxInfo
            -> (tmpDir : String)
            -> (outputDir : String)
            -> ClosedTerm
            -> (outfile : String)
            -> Core (Maybe String)
compileExpr c syn tmpDir outputDir tm outfile =
  do es <- compileToNode c syn tm
     let out = outputDir </> outfile
     Core.writeFile out es
     pure (Just out)

executeE : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> (execDir : String) -> ClosedTerm -> Core ()
executeE defs syn dir term = do coreLift $ putStrLn "Maybe in an hour."


lazyCodegen : Codegen
lazyCodegen = MkCG compileExpr executeE Nothing Nothing

main : IO ()
main = mainWithCodegens [("pygen", lazyCodegen)]


--main : IO ()
--main = do
--  putStrLn "ocas"
  

-- Local Variables:
-- idris2-load-packages: ("network" "idris2" "contrib" "base")
-- End:
