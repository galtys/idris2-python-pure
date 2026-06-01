<?php
/**
 * Test using Idris2-generated PHP code as a library.
 *
 * Usage:
 *   1. Compile: idris2-php8 --build tests/test-php.ipkg
 *   2. Run: php tests/test_lib.php
 */

$generated_file = __DIR__ . "/build/exec/test_all.php";

if (!file_exists($generated_file)) {
    echo "ERROR: Generated file not found: $generated_file\n";
    echo "Run: idris2-php8 --build tests/test-php.ipkg\n";
    exit(1);
}

ob_start();
require_once $generated_file;
$main_output = ob_get_clean();

echo "=== PHP Library Import Test ===\n\n";

function assert_test(string $label, $expected, $actual): void {
    if ($expected === $actual) {
        echo "  PASS: $label\n";
    } else {
        echo "  FAIL: $label (expected " . var_export($expected, true)
             . ", got " . var_export($actual, true) . ")\n";
    }
}

// Test factorial
$fn = $GLOBALS['libFactorial'] ?? null;
if ($fn) {
    $result = $fn(5);
    echo "factorial(5) = $result\n";
    assert_test("factorial", 120, $result);
} else {
    echo "  SKIP: factorial not found\n";
}

// Test fibonacci
$fn = $GLOBALS['libFibonacci'] ?? null;
if ($fn) {
    $result = $fn(10);
    echo "fibonacci(10) = $result\n";
    assert_test("fibonacci", 55, $result);
} else {
    echo "  SKIP: fibonacci not found\n";
}

// Test isPalindrome
$fn = $GLOBALS['libIsPalindrome'] ?? null;
if ($fn) {
    $r1 = $fn("racecar");
    $r2 = $fn("hello");
    echo "isPalindrome('racecar') = $r1\n";
    echo "isPalindrome('hello') = $r2\n";
    assert_test("isPalindrome true", 1, $r1);
    assert_test("isPalindrome false", 0, $r2);
} else {
    echo "  SKIP: isPalindrome not found\n";
}

// Test makeAdder (closure — validates PHP use() capture)
$fn = $GLOBALS['libMakeAdder'] ?? null;
if ($fn) {
    $result = $fn(5, 10);
    echo "makeAdder(5, 10) = $result\n";
    assert_test("makeAdder (closure)", 15, $result);
} else {
    echo "  SKIP: makeAdder not found\n";
}

// Test nestedClosure
$fn = $GLOBALS['TestAll_nestedClosure'] ?? null;
if ($fn) {
    $result = $fn(2);
    echo "nestedClosure(2) = $result\n";
    assert_test("nestedClosure", 10, $result);
} else {
    echo "  SKIP: nestedClosure not found\n";
}

// Test multiCapture
$fn = $GLOBALS['TestAll_multiCapture'] ?? null;
if ($fn) {
    $result = $fn(2, 3, 30);
    echo "multiCapture(2,3,30) = $result\n";
    assert_test("multiCapture", 53, $result);
} else {
    echo "  SKIP: multiCapture not found\n";
}

echo "\n=== PHP Library Import Test Complete ===\n";
