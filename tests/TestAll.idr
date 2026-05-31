module TestAll

import Data.List
import Data.String

--------------------------------------------------------------------------------
--          Assertion Helper
--------------------------------------------------------------------------------

assert : String -> Bool -> IO ()
assert label True  = putStrLn $ "  PASS: " ++ label
assert label False = putStrLn $ "  FAIL: " ++ label

assertEq : Show a => Eq a => String -> a -> a -> IO ()
assertEq label expected actual =
  if expected == actual
     then putStrLn $ "  PASS: " ++ label
     else putStrLn $ "  FAIL: " ++ label
          ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

--------------------------------------------------------------------------------
--          Numerical
--------------------------------------------------------------------------------

factorial : Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci : Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

gcd' : Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)

testNumerical : IO ()
testNumerical = do
  putStrLn "=== Numerical ==="
  assertEq "factorial 0"  1       (factorial 0)
  assertEq "factorial 5"  120     (factorial 5)
  assertEq "factorial 10" 3628800 (factorial 10)
  assertEq "fibonacci 0"  0       (fibonacci 0)
  assertEq "fibonacci 10" 55      (fibonacci 10)
  assertEq "gcd 12 8"     4       (gcd' 12 8)
  assertEq "gcd 100 75"   25      (gcd' 100 75)
  assertEq "arithmetic"   42      (2 + 5 * 8)
  assertEq "negative"     (-3)    (5 - 8)
  assertEq "division"     3       (div 7 2)

--------------------------------------------------------------------------------
--          Strings
--------------------------------------------------------------------------------

repeatStr : Nat -> String -> String
repeatStr Z     _ = ""
repeatStr (S n) s = s ++ repeatStr n s

isPalindrome : String -> Bool
isPalindrome s = s == reverse s

countChar : Char -> String -> Nat
countChar c = length . filter (== c) . unpack

testStrings : IO ()
testStrings = do
  putStrLn "=== Strings ==="
  assertEq "length"      5     (Prelude.String.length "hello")
  assertEq "append"      "foobar" ("foo" ++ "bar")
  assertEq "reverse"     "olleh"  (reverse "hello")
  assertEq "repeat"      "abcabcabc" (repeatStr 3 "abc")
  assert   "palindrome"  (isPalindrome "racecar")
  assert   "not palindrome" (not $ isPalindrome "hello")
  assertEq "countChar"   3     (countChar 'l' "hello world")
  assertEq "empty string" "" ""
  assertEq "singleton"   "x" (singleton 'x')

--------------------------------------------------------------------------------
--          Lists
--------------------------------------------------------------------------------

sum' : List Int -> Int
sum' []        = 0
sum' (x :: xs) = x + sum' xs

product' : List Int -> Int
product' []        = 1
product' (x :: xs) = x * product' xs

enumerate : List a -> List (Nat, a)
enumerate = go 0
  where
    go : Nat -> List a -> List (Nat, a)
    go _ []        = []
    go n (x :: xs) = (n, x) :: go (S n) xs

zipWith' : (a -> b -> c) -> List a -> List b -> List c
zipWith' _ []        _         = []
zipWith' _ _         []        = []
zipWith' f (x :: xs) (y :: ys) = f x y :: zipWith' f xs ys

testLists : IO ()
testLists = do
  putStrLn "=== Lists ==="
  let xs = [1, 2, 3, 4, 5]
  assertEq "sum"       15  (TestAll.sum' xs)
  assertEq "product"   120 (TestAll.product' xs)
  assertEq "length"    5   (length xs)
  assertEq "map"       [2,4,6,8,10] (map (*2) xs)
  assertEq "filter"    [2,4] (filter (\x => mod x 2 == 0) xs)
  assertEq "reverse"   [5,4,3,2,1] (reverse xs)
  assertEq "take"      [1,2,3] (take 3 xs)
  assertEq "drop"      [4,5] (drop 3 xs)
  assertEq "zipWith"   [5,7,9] (zipWith' (+) [1,2,3] [4,5,6])
  assertEq "empty sum" 0 (sum' [])
  assertEq "concat"    [1,2,3,4] ([1,2] ++ [3,4])

--------------------------------------------------------------------------------
--          Closures and Higher-Order Functions
--------------------------------------------------------------------------------

applyTwice : (a -> a) -> a -> a
applyTwice f x = f (f x)

compose : (b -> c) -> (a -> b) -> a -> c
compose g f x = g (f x)

addN : Int -> Int -> Int
addN n x = x + n

makeAdder : Int -> (Int -> Int)
makeAdder n = \x => n + x

closureCapture : Int -> Int -> Int
closureCapture x y =
  let f = \z => x + y + z
  in f 10

nestedClosure : Int -> Int
nestedClosure x =
  let f = \y => let g = \z => x + y + z in g 5
  in f 3

closureOverList : Int -> List Int -> List Int
closureOverList n xs = map (\x => x + n) xs

multiCapture : Int -> Int -> Int -> Int
multiCapture a b c =
  let f = \x => a * x + b
      g = \x => f x + c
  in g 10

testClosures : IO ()
testClosures = do
  putStrLn "=== Closures ==="
  assertEq "applyTwice (+3)"  16  (applyTwice (+3) 10)
  assertEq "compose"          25  (compose (*5) (+2) 3)
  assertEq "addN"             15  (addN 5 10)
  assertEq "makeAdder"        17  (makeAdder 7 10)
  assertEq "closureCapture"   18  (closureCapture 3 5)
  assertEq "nestedClosure"    10  (nestedClosure 2)
  assertEq "closureOverList"  [11,12,13] (closureOverList 10 [1,2,3])
  assertEq "multiCapture"     53  (multiCapture 2 3 30)
  assertEq "applyTwice (*2)"  40  (applyTwice (*2) 10)

--------------------------------------------------------------------------------
--          Pattern Matching / ADTs
--------------------------------------------------------------------------------

data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double

area : Shape -> Double
area (Circle r)        = 3.14159265 * r * r
area (Rectangle w h)   = w * h
area (Triangle a b c)  =
  let s = (a + b + c) / 2.0
  in sqrt (s * (s - a) * (s - b) * (s - c))

data Tree a = Leaf | Node (Tree a) a (Tree a)

insertBST : Ord a => a -> Tree a -> Tree a
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node l v r) =
  if x < v then Node (insertBST x l) v r
  else if x > v then Node l v (insertBST x r)
  else Node l v r

treeToList : Tree a -> List a
treeToList Leaf = []
treeToList (Node l v r) = treeToList l ++ [v] ++ treeToList r

treeSize : Tree a -> Nat
treeSize Leaf = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

testADTs : IO ()
testADTs = do
  putStrLn "=== ADTs ==="
  assert "circle area" (area (Circle 5.0) > 78.0 && area (Circle 5.0) < 79.0)
  assertEq "rectangle area" 20.0 (area (Rectangle 4.0 5.0))
  let bst = foldl (flip insertBST) Leaf [5,3,7,1,4,6,8]
  assertEq "BST sorted" [1,3,4,5,6,7,8] (treeToList bst)
  assertEq "BST size"   7 (treeSize bst)
  assertEq "BST insert dup" [1,3,4,5,6,7,8] (treeToList $ insertBST 5 bst)

--------------------------------------------------------------------------------
--          Maybe / Either
--------------------------------------------------------------------------------

safeDivide : Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (div a b)

parseDigit : Char -> Either String Int
parseDigit '0' = Right 0
parseDigit '1' = Right 1
parseDigit '2' = Right 2
parseDigit '3' = Right 3
parseDigit '4' = Right 4
parseDigit '5' = Right 5
parseDigit '6' = Right 6
parseDigit '7' = Right 7
parseDigit '8' = Right 8
parseDigit '9' = Right 9
parseDigit c   = Left ("Not a digit: " ++ singleton c)

testMaybeEither : IO ()
testMaybeEither = do
  putStrLn "=== Maybe/Either ==="
  assertEq "safe div ok"   (Just 5)    (safeDivide 10 2)
  assertEq "safe div zero" Nothing     (safeDivide 10 0)
  assertEq "parse digit"   (Right 7)   (parseDigit '7')
  assert   "parse non-digit" (case parseDigit 'x' of Left _ => True; _ => False)
  assertEq "maybe map"     (Just 10)   (map (*2) (Just 5))
  assertEq "maybe bind"    (Just 20)   (safeDivide 20 4 >>= safeDivide' 100)
  where
    safeDivide' : Int -> Int -> Maybe Int
    safeDivide' a b = safeDivide a b

--------------------------------------------------------------------------------
--          Tail Recursion (TCO stress test)
--------------------------------------------------------------------------------

sumTo : Int -> Int -> Int
sumTo acc 0 = acc
sumTo acc n = sumTo (acc + n) (n - 1)

lengthTR : List a -> Int -> Int
lengthTR []        acc = acc
lengthTR (_ :: xs) acc = lengthTR xs (acc + 1)

testTailRec : IO ()
testTailRec = do
  putStrLn "=== Tail Recursion ==="
  assertEq "sumTo 1000"  500500  (sumTo 0 1000)
  assertEq "sumTo 10000" 50005000 (sumTo 0 10000)
  assertEq "lengthTR"    100     (lengthTR (replicate 100 ()) 0)

--------------------------------------------------------------------------------
--          Library exports (callable from Python/PHP)
--------------------------------------------------------------------------------

export
libFactorial : Int -> Int
libFactorial = factorial

export
libFibonacci : Int -> Int
libFibonacci = fibonacci

export
libReverse : String -> String
libReverse = reverse

export
libIsPalindrome : String -> Bool
libIsPalindrome = isPalindrome

export
libSum : List Int -> Int
libSum = TestAll.sum'

export
libMakeAdder : Int -> Int -> Int
libMakeAdder = makeAdder

--------------------------------------------------------------------------------
--          Main
--------------------------------------------------------------------------------

main : IO ()
main = do
  putStrLn "Running Idris2 backend tests\n"
  testNumerical
  testStrings
  testLists
  testClosures
  testADTs
  testMaybeEither
  testTailRec
  putStrLn "\nAll tests completed."
