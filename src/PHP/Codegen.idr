module PHP.Codegen

import Compiler.Common
import Core.CompileExpr
import Core.Context
import Core.Context.Log
import Core.Directory
import Core.Options
import Data.List1
import Data.List
import Data.String
import Py.Ast
import Py.Doc
import Py.ToAst
import Compiler.ES.TailRec
import Py.State
import Compiler.NoMangle
import Libraries.Data.SortedMap
import Protocol.Hex
import Libraries.Data.String.Extra
import Data.Vect
import Idris.Syntax
import Idris.Pretty.Annotations
import Idris.Doc.String
import Core.Name.Namespace

%hide Libraries.Text.PrettyPrint.Prettyprinter.Doc.infixr.(<++>)

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

breakDrop1 : Char -> String -> (String, String)
breakDrop1 c = mapSnd (drop 1) . break (== c)

stringList : List String -> String
stringList = fastConcat . intersperse "," . map show

--------------------------------------------------------------------------------
--          PHP Strings
--------------------------------------------------------------------------------

-- Encode a character inside a double-quoted PHP string.
phpChar : Char -> String
phpChar c = if (c >= ' ') && (c /= '\\') && (c /= '"') && (c <= '~')
              then cast c
              else case c of
                '\0' => "\\0"
                '"'  => "\\\""
                '\\' => "\\\\"
                '\r' => "\\r"
                '\n' => "\\n"
                '\t' => "\\t"
                other => "\\u{" ++ asHex (cast c) ++ "}"

phpString : String -> String
phpString s = "\"" ++ concatMap phpChar (unpack s) ++ "\""

phpStringDoc : String -> Doc
phpStringDoc = Text . phpString

esName : String -> String
esName x = "_" ++ x

-- Convert an Idris name part to a PHP identifier.
phpIdent : String -> String
phpIdent s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar '_' = "_"
    okchar c = if isAlphaNum c then cast c else "x" ++ asHex (cast c)

phpReservedNames : List String
phpReservedNames =
  [ "abstract", "and", "array", "as", "break", "callable", "case", "catch"
  , "class", "clone", "const", "continue", "declare", "default", "die", "do"
  , "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach"
  , "endif", "endswitch", "endwhile", "enum", "eval", "exit", "extends"
  , "final", "finally", "fn", "for", "foreach", "from", "function", "global"
  , "goto", "if", "implements", "include", "include_once", "instanceof"
  , "insteadof", "interface", "list", "match", "namespace", "new", "null"
  , "or", "print", "private", "protected", "public", "readonly", "require"
  , "require_once", "return", "static", "switch", "throw", "trait", "try"
  , "unset", "use", "var", "while", "xor", "yield", "true", "false" ]

keywordSafe : String -> String
keywordSafe s = if s `elem` phpReservedNames then s ++ "_was_keyword" else s

--------------------------------------------------------------------------------
--          PHP Name Mangling
--------------------------------------------------------------------------------

phpUserName : UserName -> String
phpUserName (Basic n) = keywordSafe $ phpIdent n
phpUserName (Field n) = "rf__" ++ phpIdent n
phpUserName Underscore = keywordSafe $ phpIdent "_"

phpMangleName : Name -> String
phpMangleName (NS ns n) = phpIdent (showNSWithSep "_" ns) ++ "_" ++ phpMangleName n
phpMangleName (UN n) = phpUserName n
phpMangleName (MN n i) = phpIdent n ++ "_" ++ show i
phpMangleName (PV n d) = "pat__" ++ phpMangleName n
phpMangleName (DN _ n) = phpMangleName n
phpMangleName (Nested (i, x) n) = "n__" ++ show i ++ "_" ++ show x ++ "_" ++ phpMangleName n
phpMangleName (CaseBlock x y) = "case__" ++ phpIdent x ++ "_" ++ show y
phpMangleName (WithBlock x y) = "with__" ++ phpIdent x ++ "_" ++ show y
phpMangleName (Resolved i) = "fn__" ++ show i

parameters (noMangle : NoMangleMap)
  phpName : Name -> String
  phpName n = case isNoMangle noMangle n of
    Just name => name
    Nothing   => phpMangleName n

  -- Render a global reference as $GLOBALS['name']
  phpGlobal : String -> Doc
  phpGlobal n = Text $ "$GLOBALS['" ++ n ++ "']"

  -- Render any Var:
  --  VLoc  -> local PHP variable  $_lHEX
  --  VRef  -> $GLOBALS['_rHEX']  (minimal mode; not usually created in compact)
  --  VName -> $GLOBALS['name']   (in compact mode VName always means global ref)
  phpVar : Var -> Doc
  phpVar (VName x) = phpGlobal (phpName x)
  phpVar (VLoc x)  = Text $ "$_l" ++ asHex (cast x)
  phpVar (VRef x)  = phpGlobal ("_r" ++ asHex (cast x))

  phpVarDoc : Name -> Doc
  phpVarDoc = phpGlobal . phpName

  phpMinimal : Minimal -> Doc
  phpMinimal (MVar v)          = phpVar v
  phpMinimal (MProjection n v) = phpMinimal v <+> "['a" <+> shown n <+> "']"

mainExpr : Name
mainExpr = MN "__mainExpression" 0

--------------------------------------------------------------------------------
--          Pretty Printing
--------------------------------------------------------------------------------

-- Render $GLOBALS['name'] for the lhs of a top-level definition.
phpGlobalLhs : (noMangle : NoMangleMap) -> Var -> Doc
phpGlobalLhs nm v = phpVar nm v

tag2es : Either Int Name -> Doc
tag2es (Left x)  = shown x
tag2es (Right x) = phpStringDoc $ show x

phpConstant : Doc -> Doc -> Doc
phpConstant n d = n <+> softEq <+> d <+> ";"

applyList : (lparen : Doc) -> (rparen : Doc) -> (sep : Doc) -> List Doc -> Doc
applyList l r sep ds = l <+> (concat $ intersperse sep ds) <+> r

-- PHP array constructor tags: 'a1' => val, 'a2' => val, …
conTags : List Doc -> List Doc
conTags as = zipWith (\i,a => hcat ["'a", shown i, "' =>", SoftSpace, a]) [1..length as] as

applyObj : List Doc -> Doc
applyObj = applyList "[" "]" softComma

applyCon : ConInfo -> (tag : Either Int Name) -> (args : List Doc) -> Doc
applyCon NIL     _ [] = "['h_x' => 0]"
applyCon NOTHING _ [] = "['h_x' => 0]"
applyCon CONS    _ as = applyObj (conTags as)
applyCon JUST    _ as = applyObj (conTags as)
applyCon RECORD  _ as = applyObj (conTags as)
applyCon UNIT    _ [] = "null"
applyCon _       t as = applyObj (("'h_x' =>" <+> SoftSpace <+> tag2es t) :: conTags as)

app : (fun : Doc) -> (args : List Doc) -> Doc
app fun args = fun <+> applyList "(" ")" softComma args

callFun : String -> List Doc -> Doc
callFun = app . Text

callFun1 : String -> Doc -> Doc
callFun1 fun = callFun fun . pure

phpCrashExp : (msg : Doc) -> Doc
phpCrashExp = callFun1 (esName "crashExp")

-- Generate a PHP closure assigned to a $GLOBALS slot:
--   $GLOBALS['name'] = function($a, $b) { body };
phpFunction : (name : Doc) -> (args : List Doc) -> (body : Doc) -> Doc
phpFunction n args body =
  n <++> "=" <++> "function" <+> applyList "(" ")" softComma args <++> "{" <+> block body <+> "};"

phpFunctionWithUse : (name : Doc) -> (args : List Doc) -> (useVars : List Doc) -> (body : Doc) -> Doc
phpFunctionWithUse n args [] body = phpFunction n args body
phpFunctionWithUse n args useVars body =
  n <++> "=" <++> "function" <+> applyList "(" ")" softComma args
  <++> "use" <+> applyList "(" ")" softComma useVars
  <++> "{" <+> block body <+> "};"

--------------------------------------------------------------------------------
--          Primitives
--------------------------------------------------------------------------------

toBigInt : Doc -> Doc
toBigInt = callFun1 "intval"

fromBigInt : Doc -> Doc
fromBigInt = callFun1 "intval"

useBigInt' : Int -> Bool
useBigInt' = (> 32)

useBigInt : IntKind -> Bool
useBigInt (Signed $ P x)     = useBigInt' x
useBigInt (Signed Unlimited) = True
useBigInt (Unsigned x)       = useBigInt' x

phpBigIntOfString : Doc -> Doc
phpBigIntOfString = callFun1 (esName "bigIntOfString")

phpNumberOfString : Doc -> Doc
phpNumberOfString = callFun1 (esName "numberOfString")

phpIntOfString : IntKind -> Doc -> Doc
phpIntOfString k =
  if useBigInt k then phpBigIntOfString else callFun1 (esName "intOfString")

binOp : (symbol : String) -> (lhs : Doc) -> (rhs : Doc) -> Doc
binOp sym lhs rhs = hcat ["(", lhs, Text sym, rhs, ")"]

toInt : IntKind -> Doc -> Doc
toInt k = if useBigInt k then toBigInt else id

fromInt : IntKind -> Doc -> Doc
fromInt k = if useBigInt k then fromBigInt else id

-- PHP: mb_ord returns code point; for ASCII $x[0] also works but mb_ord is correct.
phpIntOfChar : IntKind -> Doc -> Doc
phpIntOfChar k s = toInt k $ callFun1 "mb_ord" s

phpIntOfDouble : IntKind -> Doc -> Doc
phpIntOfDouble k = toInt k . callFun1 "intval"

phpAnyToString : Doc -> Doc
phpAnyToString s = "((string)(" <+> s <+> "))"

phpCharOfInt : IntKind -> Doc -> Doc
phpCharOfInt k = callFun1 (esName "truncToChar") . fromInt k

truncateSigned : (isBigInt : Bool) -> (bits : Int) -> (int : Doc) -> Doc
truncateSigned isBigInt bits =
  let add = if isBigInt then "BigInt" else "Int"
   in callFun1 (esName "trunc" ++ add ++ show bits)

truncateUnsigned : (isBigInt : Bool) -> (bits : Int) -> (int : Doc) -> Doc
truncateUnsigned isBigInt bits =
  let add = if isBigInt then "BigInt" else "Int"
   in callFun1 (esName "truncU" ++ add ++ show bits)

integerOp : (op : String) -> (lhs : Doc) -> (rhs : Doc) -> Doc
integerOp op x y = callFun (fastConcat ["_", op, "BigInt"]) [x,y]

boundedOp : (suffix : String) -> (bits : Int) -> (op : String) -> (lhs : Doc) -> (rhs : Doc) -> Doc
boundedOp s bits o x y = callFun (fastConcat ["_", o, show bits, s]) [x,y]

boundedIntOp : Int -> String -> Doc -> Doc -> Doc
boundedIntOp = boundedOp "s"

boundedUIntOp : Int -> String -> Doc -> Doc -> Doc
boundedUIntOp = boundedOp "u"

boolOp : (op : String) -> (lhs : Doc) -> (rhs : Doc) -> Doc
boolOp o lhs rhs = "(" <+> binOp o lhs rhs <+> ")"

phpPrimType : PrimType -> String
phpPrimType _ = "#t"

phpIntKind : PrimType -> Maybe IntKind
phpIntKind IntType = Just . Signed $ P 32
phpIntKind x       = intKind x

phpMod : PrimType -> Doc -> Doc -> Doc
phpMod ty x y = case phpIntKind ty of
  (Just $ Signed $ P n) => case useBigInt' n of
    True  => integerOp "mod" x y
    False => callFun "_mod" [x,y]
  (Just $ Unsigned n)   => binOp "%" x y
  _                     => integerOp "mod" x y

arithOp : Maybe IntKind -> (sym : String) -> (op : String) -> (lhs : Doc) -> (rhs : Doc) -> Doc
arithOp (Just $ Signed $ P n)     _   op = boundedIntOp n op
arithOp (Just $ Unsigned n)       _   op = boundedUIntOp n op
arithOp (Just $ Signed Unlimited) ""  op = integerOp op
arithOp _                         sym _  = binOp sym

phpConstant' : Constant -> String
phpConstant' (I i)    = show i
phpConstant' (I8 i)   = show i
phpConstant' (I16 i)  = show i
phpConstant' (I32 i)  = show i
phpConstant' (I64 i)  = show i
phpConstant' (BI i)   = show i
phpConstant' (B8 i)   = show i
phpConstant' (B16 i)  = show i
phpConstant' (B32 i)  = show i
phpConstant' (B64 i)  = show i
phpConstant' (Str s)  = phpString s
phpConstant' (Ch c)   = phpString $ singleton c
phpConstant' (Db f)   = show f
phpConstant' (PrT t)  = phpPrimType t
phpConstant' WorldVal = "null"

castInt : PrimType -> PrimType -> Doc -> Core Doc
castInt from to x =
  case ((from, phpIntKind from), (to, phpIntKind to)) of
    ((CharType,_),  (_,Just k)) => truncInt (useBigInt k) k $ phpIntOfChar k x
    ((StringType,_),(_,Just k)) => truncInt (useBigInt k) k (phpIntOfString k x)
    ((DoubleType,_),(_,Just k)) => truncInt (useBigInt k) k $ phpIntOfDouble k x
    ((_,Just k),(CharType,_))   => pure $ phpCharOfInt k x
    ((_,Just k),(StringType,_)) => pure $ phpAnyToString x
    ((_,Just k),(DoubleType,_)) => pure $ fromInt k x
    ((_,Just k1),(_,Just k2))   => intImpl k1 k2
    _ => errorConcat $ ["invalid cast: ",show from," -> ",show to]
  where
    truncInt : (isBigInt : Bool) -> IntKind -> Doc -> Core Doc
    truncInt b (Signed Unlimited) = pure
    truncInt b (Signed $ P n)     = pure . truncateSigned b n
    truncInt b (Unsigned n)       = pure . truncateUnsigned b n

    shrink : IntKind -> IntKind -> Doc -> Doc
    shrink k1 k2 = case (useBigInt k1, useBigInt k2) of
                        (True, False) => fromBigInt
                        _             => id

    expand : IntKind -> IntKind -> Doc -> Doc
    expand k1 k2 = case (useBigInt k1, useBigInt k2) of
                        (False,True) => toBigInt
                        _            => id

    intImpl : IntKind -> IntKind -> Core Doc
    intImpl k1 k2 =
      let expanded = expand k1 k2 x
          shrunk   = shrink k1 k2 <$> truncInt (useBigInt k1) k2 x
       in case (k1,k2) of
            (_, Signed Unlimited)    => pure expanded
            (Signed m, Signed n)     =>
              if n >= m then pure expanded else shrunk
            (Signed _, Unsigned n)   =>
              case (useBigInt k1, useBigInt k2) of
                   (False,True)  => truncInt True k2 (toBigInt x)
                   _             => shrunk
            (Unsigned m, Unsigned n) =>
              if n >= m then pure expanded else shrunk
            (Unsigned m, Signed n)   =>
              if n > P m then pure expanded else shrunk

phpOp : {0 arity : Nat} -> PrimFn arity -> Vect arity Doc -> Core Doc
phpOp (Add ty) [x, y] = pure $ arithOp (phpIntKind ty) "+" "add" x y
phpOp (Sub ty) [x, y] = pure $ arithOp (phpIntKind ty) "-" "sub" x y
phpOp (Mul ty) [x, y] = pure $ arithOp (phpIntKind ty) "*" "mul" x y
phpOp (Div DoubleType) [x, y] = pure $ binOp "/" x y
phpOp (Div ty) [x, y] = pure $ arithOp (phpIntKind ty) ""  "div" x y
phpOp (Mod ty) [x, y] = pure $ phpMod ty x y
phpOp (Neg ty) [x] = pure $ "(-(" <+> x <+> "))"
phpOp (ShiftL Int32Type) [x, y] = pure $ binOp "<<" x y
phpOp (ShiftL IntType)   [x, y] = pure $ binOp "<<" x y
phpOp (ShiftL ty) [x, y] = pure $ arithOp (phpIntKind ty) "<<" "shl" x y
phpOp (ShiftR Int32Type) [x, y] = pure $ binOp ">>" x y
phpOp (ShiftR IntType)   [x, y] = pure $ binOp ">>" x y
phpOp (ShiftR ty) [x, y] = pure $ arithOp (phpIntKind ty) ">>" "shr" x y
phpOp (BAnd Bits32Type) [x, y] = pure $ boundedUIntOp 32 "and" x y
phpOp (BOr  Bits32Type) [x, y] = pure $ boundedUIntOp 32 "or"  x y
phpOp (BXOr Bits32Type) [x, y] = pure $ boundedUIntOp 32 "xor" x y
phpOp (BAnd ty) [x, y] = pure $ binOp "&" x y
phpOp (BOr  ty) [x, y] = pure $ binOp "|" x y
phpOp (BXOr ty) [x, y] = pure $ binOp "^" x y
phpOp (LT ty)  [x, y] = pure $ boolOp "<"  x y
phpOp (LTE ty) [x, y] = pure $ boolOp "<=" x y
phpOp (EQ ty)  [x, y] = pure $ boolOp "==" x y
phpOp (GTE ty) [x, y] = pure $ boolOp ">=" x y
phpOp (GT ty)  [x, y] = pure $ boolOp ">"  x y
-- PHP string ops
phpOp StrLength [x]            = pure $ callFun1 "mb_strlen" x
phpOp StrHead   [x]            = pure $ callFun "mb_substr" [x, "0", "1"]
phpOp StrTail   [x]            = pure $ callFun "mb_substr" [x, "1"]
phpOp StrIndex  [x, y]         = pure $ callFun "mb_substr" [x, y, "1"]
phpOp StrCons   [x, y]         = pure $ binOp "." x y
phpOp StrAppend [x, y]         = pure $ binOp "." x y
phpOp StrReverse [x]           = pure $ callFun1 (esName "strReverse") x
phpOp StrSubstr [offset,len,str] = pure $ callFun (esName "substr") [offset,len,str]
-- PHP math ops (built-in, no module prefix needed)
phpOp DoubleExp  [x]    = pure $ callFun1 "exp"   x
phpOp DoubleLog  [x]    = pure $ callFun1 "log"   x
phpOp DoublePow  [x, y] = pure $ callFun "pow"    [x, y]
phpOp DoubleSin  [x]    = pure $ callFun1 "sin"   x
phpOp DoubleCos  [x]    = pure $ callFun1 "cos"   x
phpOp DoubleTan  [x]    = pure $ callFun1 "tan"   x
phpOp DoubleASin [x]    = pure $ callFun1 "asin"  x
phpOp DoubleACos [x]    = pure $ callFun1 "acos"  x
phpOp DoubleATan [x]    = pure $ callFun1 "atan"  x
phpOp DoubleSqrt [x]    = pure $ callFun1 "sqrt"  x
phpOp DoubleFloor [x]   = pure $ callFun1 "floor" x
phpOp DoubleCeiling [x] = pure $ callFun1 "ceil"  x
phpOp (Cast StringType DoubleType) [x] = pure $ phpNumberOfString x
phpOp (Cast ty StringType) [x] = pure $ phpAnyToString x
phpOp (Cast ty ty2) [x]        = castInt ty ty2 x
phpOp BelieveMe [_,_,x] = pure x
phpOp Crash [_, msg] = pure $ phpCrashExp msg

--------------------------------------------------------------------------------
--          FFI
--------------------------------------------------------------------------------

readCCPart : String -> (String, String)
readCCPart = breakDrop1 ':'

searchForeign : List String -> List String -> Either (List String) String
searchForeign knownBackends decls =
  let pairs    = map readCCPart decls
      backends = Left $ map fst pairs
   in maybe backends (Right . snd) $ find ((`elem` knownBackends) . fst) pairs

makeForeign :  {auto d : Ref Ctxt Defs}
            -> {auto c : Ref ESs ESSt}
            -> {auto nm : Ref NoMangleMap NoMangleMap}
            -> (name : Name)
            -> (ffDecl : String)
            -> Core Doc
makeForeign n x = do
  nd <- phpVar !(get NoMangleMap) <$> getOrRegisterRef n
  let (ty, def) = readCCPart x
  case ty of
    "lambda" => pure . phpConstant nd . paren $ Text def
    "stringIterator" =>
      case def of
        "new"      => pure $ phpConstant nd "__prim_stringIteratorNew"
        "next"     => pure $ phpConstant nd "__prim_stringIteratorNext"
        "toString" => pure $ phpConstant nd "__prim_stringIteratorToString"
        _ => errorConcat
               [ "Invalid string iterator function: ", def, ". "
               , "Supported functions are: "
               , stringList ["new","next","toString"], "."
               ]
    _ => errorConcat
           [ "Invalid foreign type: ", ty, ". "
           , "Supported types are: "
           , stringList ["lambda", "stringIterator"]
           ]

fpName : String -> String -> Name
fpName mod fn = NS ns (UN (Basic fn)) where
   ns : Namespace
   ns = mkNamespace mod

-- PHP-specific built-in FFI bindings (injected before user-supplied ones).
addForeign : List (Name, String)
addForeign =
  [ (fpName "Prelude.Types" "fastConcat",
       "php8:lambda: fn($xs) => fastConcat($xs)")
  , (fpName "Prelude.Types" "fastUnpack",
       "php8:lambda: fn($x) => php_support_fastUnpack($x)")
  , (fpName "Prelude.Types" "fastPack",
       "php8:lambda: fn($x) => php_support_fastPack($x)")
  , (fpName "Prelude.IO"    "prim__putStr",
       "php8:lambda: fn($x, $world) => print_obj($x)")
  , (fpName "PrimIO"        "prim__nullAnyPtr",
       "php8:lambda: fn($x) => php_support_isNone($x)")
  , (fpName "System.Errno"  "prim__strerror",
       "php8:lambda: fn($e) => _strerror((int)$e)")
  , (fpName "System.File.ReadWrite" "prim__writeLine",
       "php8:lambda: fn($f, $s, $world) => php_support_writeLine($f, $s)")
  , (fpName "System.File.Handle" "prim__open",
       "php8:lambda: fn($f, $m, $world) => fopen($f, $m)")
  , (fpName "System.File.Handle" "prim__close",
       "php8:lambda: fn($f, $world) => fclose($f)")
  , (fpName "System" "prim__getArgCount",
       "php8:lambda: fn($w) => prim_getArgCount()")
  , (fpName "System" "prim__getArg",
       "php8:lambda: fn($n, $w) => prim_getArg($n)")
  ]

foreignDecl :  {auto d : Ref Ctxt Defs}
            -> {auto c : Ref ESs ESSt}
            -> {auto nm : Ref NoMangleMap NoMangleMap}
            -> Name
            -> List String
            -> Core Doc
foreignDecl n ccs = do
  tys <- ccTypes <$> get ESs
  let new_ccs = case List.lookup n addForeign of
                   Nothing    => ccs
                   (Just php) => (php :: ccs)
  case searchForeign tys new_ccs of
    Right x       => makeForeign n x
    Left backends =>
      errorConcat
        [ "No supported backend found in the definition of ", show n, ". "
        , "Supported backends: ", stringList tys, ". "
        , "Backends in definition: ", stringList backends, "."
        ]

phpPrim : {auto c : Ref ESs ESSt} -> Name -> List Doc -> Core Doc
phpPrim nm docs = case (dropAllNS nm, docs) of
  (UN (Basic "prim__newIORef"),   [_,v,_])     => pure $ callFun "_prim_newIORef"   [v]
  (UN (Basic "prim__readIORef"),  [_,r,_])     => pure $ callFun "_prim_readIORef"  [r]
  (UN (Basic "prim__writeIORef"), [_,r,v,_])   => pure $ callFun "_prim_writeIORef" [r,v]
  (UN (Basic "prim__newArray"),   [_,s,v,_])   => pure $ callFun "newArray" [s,v]
  (UN (Basic "prim__arrayGet"),   [_,x,p,_])   =>
      pure $ hcat ["($GLOBALS['array_dict'][", x, "][", p, "])"]
  (UN (Basic "prim__arraySet"),   [_,x,p,v,_]) =>
      pure $ hcat ["($GLOBALS['array_dict'][", x, "][", p, "]=", v, ")"]
  (UN (Basic "void"),      [_,_]) => pure . phpCrashExp $ phpStringDoc "Error: Executed 'void'"
  (UN (Basic "prim__void"),[_,_]) => pure . phpCrashExp $ phpStringDoc "Error: Executed 'void'"
  (UN (Basic "prim__codegen"), []) => do
    (cg :: _) <- ccTypes <$> get ESs
        | _ => pure (Text (phpString "php8"))
    pure . Text $ phpString cg
  (UN (Basic "prim__os"), []) => do
    tys <- ccTypes <$> get ESs
    case searchForeign tys ["php8"] of
      Right _ => pure $ Text "PHP_OS_FAMILY"
      Left  _ => throw $ InternalError "prim not implemented: prim__os"
  _ => throw $ InternalError $ "prim not implemented: " ++ show nm

--------------------------------------------------------------------------------
--          Codegen
--------------------------------------------------------------------------------

isArg : CGMode -> Exp -> Bool
isArg Pretty (ELam _ _ $ Block _ _)           = False
isArg Pretty (ELam _ _ $ ConSwitch _ _ _ _)   = False
isArg Pretty (ELam _ _ $ ConstSwitch _ _ _ _) = False
isArg Pretty (ELam _ _ $ Error _)             = False
isArg _      _                                = True

isFun : Exp -> Bool
isFun (ELam _ _ _) = False
isFun _            = True

-- PHP if/elseif/else chain replacing a switch.
phpSwitch :  (scrutinee : Doc)
          -> (alts : List (Doc, Doc))
          -> (def : Maybe Doc)
          -> Doc
phpSwitch sc alts def =
  case alts of
    [] => defCase def
    ((e,d) :: rest) =>
      "if" <+> paren (sc <+> " === " <+> e) <+> "{" <+> block d <+> "}"
      <+> vcat (map (mkElseif sc) rest)
      <+> defCase def
  where
    defCase : Maybe Doc -> Doc
    defCase Nothing  = "else" <+> "{" <+> block "/* unreachable */;" <+> "}"
    defCase (Just d) = "else" <+> "{" <+> block d <+> "}"
    mkElseif : Doc -> (Doc, Doc) -> Doc
    mkElseif sc (e, d) = "elseif" <+> paren (sc <+> " === " <+> e) <+> "{" <+> block d <+> "}"

insertBreak : (r : Effect) -> (Doc, Doc) -> (Doc, Doc)
insertBreak _ x = x   -- PHP if/elseif needs no break

vectToList : Vect n t -> List t
vectToList [] = []
vectToList (x :: xs) = x :: vectToList xs

-- PHP arrow-function lambda (curried): fn($_l0) => fn($_l1) => ...
phpLambdaArgs : (noMangle : NoMangleMap) -> List Var -> Doc
phpLambdaArgs noMangle [] = "fn() =>"
phpLambdaArgs noMangle xs =
  hcat $ map (<+> " =>") ((\v => "fn(" <+> phpVar noMangle v <+> ")") <$> xs)

-- Comma-separated parameter list for a closure definition.
phpFArgs : (noMangle : NoMangleMap) -> List Var -> Doc
phpFArgs noMangle xs = hcat (intersperse softComma (phpVar noMangle <$> xs))

-- PHP-specific pretty printer that renders Comment nodes as /* ... */
phpPretty : Doc -> String
phpPretty = fastConcat . go ""
  where
    nSpaces : Nat -> String
    nSpaces n = fastPack $ replicate n ' '
    go : String -> Doc -> List String
    go _ Nil         = []
    go s LineBreak   = ["\n", s]
    go _ SoftSpace   = [" "]
    go s (Comment x) = ["/* "] ++ go s x ++ [" */"]
    go _ (Text x)    = [x]
    go s (Nest x y)  = go (s ++ nSpaces x) y
    go s (Seq x y)   = go s x ++ go s y

printDoc : CGMode -> Doc -> String
printDoc Pretty  y = phpPretty (y <+> LineBreak)
printDoc Compact y = compact y
printDoc Minimal y = compact y

--------------------------------------------------------------------------------
--          Free Variable Analysis (for PHP closure `use` clauses)
--------------------------------------------------------------------------------

getVLocId : Var -> Maybe Int
getVLocId (VLoc i) = Just i
getVLocId _        = Nothing

mutual
  allRefsMinimal : Minimal -> List Int
  allRefsMinimal (MVar (VLoc i))   = [i]
  allRefsMinimal (MVar _)          = []
  allRefsMinimal (MProjection _ m) = allRefsMinimal m

  allRefsExp : Exp -> List Int
  allRefsExp (EMinimal m)      = allRefsMinimal m
  allRefsExp (ELam _ xs body)  = freeVarsOfClosure xs body
  allRefsExp (EApp f args)     = allRefsExp f ++ concatMap allRefsExp args
  allRefsExp (ECon _ _ args)   = concatMap allRefsExp args
  allRefsExp (EOp _ args)      = concatMap allRefsExp (vectToList args)
  allRefsExp (EExtPrim _ args) = concatMap allRefsExp args
  allRefsExp (EPrimVal _)      = []
  allRefsExp EErased           = []

  allRefsStmt : {e : _} -> Stmt e -> List Int
  allRefsStmt (Return x)                  = allRefsExp x
  allRefsStmt (Const _ x)                 = allRefsExp x
  allRefsStmt (Assign _ x)               = allRefsExp x
  allRefsStmt (Declare _ s)              = allRefsStmt s
  allRefsStmt (ConSwitch _ sc alts def)  =
    allRefsMinimal sc
    ++ concatMap (\a => allRefsStmt a.body) alts
    ++ maybe [] allRefsStmt def
  allRefsStmt (ConstSwitch _ sc alts def) =
    allRefsExp sc
    ++ concatMap (\a => allRefsStmt a.body) alts
    ++ maybe [] allRefsStmt def
  allRefsStmt (Error _)                  = []
  allRefsStmt (Block ss s)               =
    concatMap allRefsStmt (forget ss) ++ allRefsStmt s

  allBoundStmt : {e : _} -> Stmt e -> List Int
  allBoundStmt (Const (VLoc i) _)   = [i]
  allBoundStmt (Declare (VLoc i) _) = [i]
  allBoundStmt (Block ss s)         =
    concatMap allBoundStmt (forget ss) ++ allBoundStmt s
  allBoundStmt _                    = []

  freeVarsOfClosure : List Var -> Stmt (Just Returns) -> List Int
  freeVarsOfClosure params body =
    let allRefs  = allRefsStmt body
        paramIds = mapMaybe getVLocId params
        boundIds = allBoundStmt body
        excluded = paramIds ++ boundIds
    in nub $ filter (\i => not (i `elem` excluded)) allRefs

phpVLocDoc : Int -> Doc
phpVLocDoc i = Text $ "$_l" ++ asHex (cast i)

mutual
  exp :  {auto c : Ref ESs ESSt}
      -> {auto nm : Ref NoMangleMap NoMangleMap}
      -> Exp
      -> Core Doc
  exp (EMinimal x) = pure $ phpMinimal !(get NoMangleMap) x

  exp el@(ELam no xs y) = do
    nm <- get NoMangleMap
    _ <- stmt y
    let closureRef = "$__closure_fun" ++ show no
    let lam_expr = phpLambdaArgs nm xs <+> Text closureRef <+> paren (phpFArgs nm xs)
    pure lam_expr

  exp (EApp x xs) = do
    o    <- exp x
    args <- traverse exp xs
    pure $ app o args

  exp (ECon tag ci xs) = applyCon ci tag <$> traverse exp xs

  exp (EOp x xs)     = traverseVect exp xs >>= phpOp x
  exp (EExtPrim x xs) = traverse exp xs >>= phpPrim x
  exp (EPrimVal x)   = pure . Text $ phpConstant' x
  exp EErased        = pure "$GLOBALS['py_support_erased']"

  hasELamStmt : Exp -> List (Int, List Var, Stmt (Just Returns))
  hasELamStmt (ELam no xs y) = [(no, xs, y)]
  hasELamStmt (EApp e xs)    = hasELamStmt e ++ concat (map hasELamStmt xs)
  hasELamStmt (EOp a xs)     = concat (map hasELamStmt $ vectToList xs)
  hasELamStmt (ECon _ _ xs)  = concat (map hasELamStmt xs)
  hasELamStmt (EExtPrim _ xs)= concat (map hasELamStmt xs)
  hasELamStmt _              = []

  cr_closure :  {auto c : Ref ESs ESSt}
             -> {auto nm : Ref NoMangleMap NoMangleMap}
             -> (Int, List Var, Stmt (Just Returns))
             -> Core Doc
  cr_closure (no, xs, y) = do
    nm  <- get NoMangleMap
    kky <- stmt y
    let closureName = Text $ "$__closure_fun" ++ show no
    let freeIds = freeVarsOfClosure xs y
    let useVars = map phpVLocDoc freeIds
    pure $ phpFunctionWithUse closureName (map (phpVar nm) xs) useVars kky

  stmt :  {e : _}
       -> {auto c : Ref ESs ESSt}
       -> {auto nm : Ref NoMangleMap NoMangleMap}
       -> Stmt e
       -> Core Doc
  stmt (Return xe) = do
    resx    <- ((\e => "return" <++> e <+> ";") <$> exp xe)
    nm      <- get NoMangleMap
    loc_fns <- traverse cr_closure (hasELamStmt xe)
    pure (vcat (loc_fns ++ [resx]))

  stmt (Const v x) = do
    nm      <- get NoMangleMap
    resx    <- (phpConstant (phpVar nm v) <$> exp x)
    loc_fns <- traverse cr_closure (hasELamStmt x)
    pure (vcat (loc_fns ++ [resx]))

  stmt (Declare v s) = do
    nm <- get NoMangleMap
    (\d => vcat [phpVar nm v <+> "= null;", d]) <$> stmt s

  stmt (Assign v x) = do
    nm      <- get NoMangleMap
    resx    <- ((\d => hcat [phpVar nm v, softEq, d, ";"]) <$> exp x)
    loc_fns <- traverse cr_closure (hasELamStmt x)
    pure (vcat (loc_fns ++ [resx]))

  stmt (ConSwitch r sc alts def) = do
    as <- traverse (map (insertBreak r) . alt) alts
    d  <- traverseOpt stmt def
    nm <- get NoMangleMap
    pure (phpSwitch (phpMinimal nm sc <+> "['h_x']") as d)
    where
      alt : {r : _} -> EConAlt r -> Core (Doc, Doc)
      alt (MkEConAlt _ RECORD b)  = ("\"undefined\"",) <$> stmt b
      alt (MkEConAlt _ NIL    b)  = ("0",) <$> stmt b
      alt (MkEConAlt _ CONS   b)  = ("\"undefined\"",) <$> stmt b
      alt (MkEConAlt _ NOTHING b) = ("0",) <$> stmt b
      alt (MkEConAlt _ JUST   b)  = ("\"undefined\"",) <$> stmt b
      alt (MkEConAlt _ UNIT   b)  = ("\"undefined\"",) <$> stmt b
      alt (MkEConAlt t _ b)       = (tag2es t,) <$> stmt b

  stmt (ConstSwitch r sc alts def) = do
    as      <- traverse (map (insertBreak r) . alt) alts
    d       <- traverseOpt stmt def
    ex      <- exp sc
    loc_fns <- traverse cr_closure (hasELamStmt sc)
    pure $ vcat [vcat loc_fns, phpSwitch ex as d]
    where
      alt : EConstAlt r -> Core (Doc, Doc)
      alt (MkEConstAlt c b) = do
        d <- stmt b
        pure (Text $ phpConstant' c, d)

  stmt (Error x) = pure $ Text ("throw new \\Exception(" ++ phpString x ++ ");")

  stmt (Block ss s) = do
    docs <- traverse stmt $ forget ss
    doc  <- stmt s
    pure $ vcat (docs ++ [doc])

--------------------------------------------------------------------------------
--          Top-level Definition
--------------------------------------------------------------------------------

def :  {auto c : Ref Ctxt Defs}
    -> {auto s : Ref Syn SyntaxInfo}
    -> {auto e : Ref ESs ESSt}
    -> {auto nm : Ref NoMangleMap NoMangleMap}
    -> Function
    -> Core String
def (MkFunction n as body) = do
  reset
  defs <- get Ctxt
  mty <- do log "compiler.javascript.doc" 50 $ "Looking up \{show n}"
            Just gdef <- lookupCtxtExact n (gamma defs)
              | Nothing => pure Nothing
            let UN _ = dropNS n
              | _ => pure Nothing
            ty <- prettyType (const ()) gdef.type
            pure (Just (shown ty))
  ref  <- getOrRegisterRef n
  args <- traverse registerLocal as
  mde  <- mode <$> get ESs
  b    <- stmt Returns body >>= stmt
  let cmt = comment $ hsep (shown n :: toList ((":" <++>) <$> mty))
  pure $ printDoc mde $ vcat
    [ cmt
    , phpFunction (phpVar !(get NoMangleMap) ref)
                  (map (phpVar !(get NoMangleMap)) args)
                  b
    ]

foreign :  {auto c : Ref ESs ESSt}
        -> {auto d : Ref Ctxt Defs}
        -> {auto nm : Ref NoMangleMap NoMangleMap}
        -> (Name, FC, NamedDef)
        -> Core (List String)
foreign (n, fc, MkNmForeign path _ _) = pure . pretty <$> foreignDecl n path
foreign _                             = pure []

tailRec : Name
tailRec = UN $ Basic "__tailRec"

validPhpName : String -> Bool
validPhpName name =
    not (name `elem` phpReservedNames)
    && all validNameChar (unpack name)
    && (case strM name of
      StrNil       => True
      StrCons h _  => not $ isDigit h)
  where
    validNameChar : Char -> Bool
    validNameChar c = isAlphaNum c || c == '_'

export
addSupport : List String -> List String
addSupport xs = [ ("php/" ++ x ++ ".php") | x <- xs ]

export
compileToES :  Ref Ctxt Defs
            -> Ref Syn SyntaxInfo
            -> (cg : CG)
            -> ClosedTerm
            -> List String
            -> Core String
compileToES c s cg tm ccTypes = do
  _ <- initNoMangle ccTypes validPhpName

  cdata <- getCompileDataWith ccTypes False Cases tm

  directives <- getDirectives cg
  let phpMode = Compact

  s <- newRef ESs $ init phpMode (isArg phpMode) isFun ccTypes !(get NoMangleMap)

  addRef tailRec (VName tailRec)

  let allDefs = (mainExpr, EmptyFC, MkNmFun [] $ forget cdata.mainExpr)
             :: cdata.namedDefs

      defs = TailRec.functions tailRec allDefs

  defDecls <- traverse def defs
  foreigns  <- concat <$> traverse foreign allDefs

  let add_supp = addSupport (nub directives)

  static_preamble <- readDataFile "php/php_support.php"
  supp            <- traverse readDataFile add_supp
  run_main        <- readDataFile "php/run_main.php"

  let pre   = showSep "\n" $ supp ++ [static_preamble]
      after = showSep "\n" [run_main]

  pure $ fastUnlines [pre, fastUnlines $ foreigns ++ defDecls, after]
