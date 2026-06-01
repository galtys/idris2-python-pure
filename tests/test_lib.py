#!/usr/bin/env python2
"""
Test using Idris2-generated Python code as a library.

Usage:
  1. Compile: idris2-python-pure --build tests/test-py.ipkg
  2. Run: python2 tests/test_lib.py
"""
import sys
import os

generated_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), "build", "exec", "test_all.py")

if not os.path.exists(generated_file):
    print("ERROR: Generated file not found: %s" % generated_file)
    print("Run: idris2-python-pure --build tests/test-py.ipkg")
    sys.exit(1)

ns = {"__name__": "idris_lib", "__builtins__": __builtins__}
sys.setrecursionlimit(50000)
with open(generated_file) as f:
    code = f.read()
    code = code.replace("if __name__ == '__main__':", "if False:")
    exec(compile(code, generated_file, "exec"), ns)

print("=== Python Library Import Test ===\n")

# Test factorial
fn = ns.get("libFactorial")
if fn:
    result = fn(5)
    print("factorial(5) = %s" % result)
    assert result == 120, "Expected 120, got %s" % result
    print("  PASS: factorial")
else:
    print("  SKIP: factorial not found")

# Test fibonacci
fn = ns.get("libFibonacci")
if fn:
    result = fn(10)
    print("fibonacci(10) = %s" % result)
    assert result == 55, "Expected 55, got %s" % result
    print("  PASS: fibonacci")
else:
    print("  SKIP: fibonacci not found")

# Test reverse
fn = ns.get("libIsPalindrome")
if fn:
    r1 = fn("racecar")
    r2 = fn("hello")
    print("isPalindrome('racecar') = %s" % r1)
    print("isPalindrome('hello') = %s" % r2)
    assert r1 == 1, "Expected 1 (true), got %s" % r1
    assert r2 == 0, "Expected 0 (false), got %s" % r2
    print("  PASS: isPalindrome")
else:
    print("  SKIP: isPalindrome not found")

# Test makeAdder (closure)
fn = ns.get("libMakeAdder")
if fn:
    result = fn(5, 10)
    print("makeAdder(5, 10) = %s" % result)
    assert result == 15, "Expected 15, got %s" % result
    print("  PASS: makeAdder (closure)")
else:
    print("  SKIP: makeAdder not found")

# Test nestedClosure
fn = ns.get("TestAll_nestedClosure")
if fn:
    result = fn(2)
    print("nestedClosure(2) = %s" % result)
    assert result == 10, "Expected 10, got %s" % result
    print("  PASS: nestedClosure")
else:
    print("  SKIP: nestedClosure not found")

# Test multiCapture
fn = ns.get("TestAll_multiCapture")
if fn:
    result = fn(2, 3, 30)
    print("multiCapture(2,3,30) = %s" % result)
    assert result == 53, "Expected 53, got %s" % result
    print("  PASS: multiCapture")
else:
    print("  SKIP: multiCapture not found")

print("\n=== Python Library Import Test Complete ===")
