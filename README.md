# Language Description
An imperative language based on the `Latte` language. It has three types: `int`, `bool`, and `string`. Most constructs are identical to those in `Latte`, with the difference that operations do not need to be within a function body (similar to `Python`). Additionally, variables can be passed by value and by reference.

If a function does not end with `return _;`, we assume default return values: `int` -> `0`, `bool` -> `false`, `string` -> `""`.

# Execution
`GHC` version `9.0.2` is required for execution.

## Compilation
To create an executable file, run the following command in the main directory:
```
make
```

## Program Execution
There are two ways to provide the program to the interpreter: through a file or standard input.

Via file:
```
./interpreter <path-to-file>
```

To read from standard input, simply run without any arguments:
```
./interpreter
```

# Feature Table
```
For 15 points
  01 (three types) +
  02 (literals, arithmetic, comparisons) +
  03 (variables, assignment) +
  04 (print) +
  05 (while, if) +
  06 (functions or procedures, recursion) +
  07 (by variable / by value / in/out) +
  08 (read-only variables and for loop) -
For 20 points
  09 (shadowing and static binding) +
  10 (runtime error handling) +
  11 (functions returning a value) +
For 30 points
  12 (4) (static typing) +
  13 (2) (nested functions with static binding) +
  14 (1/2) (records/lists/arrays/multidimensional arrays) -
  15 (2) (tuples with assignment) -
  16 (1) (break, continue) -
  17 (4) (higher-order functions, anonymous functions, closures) -
  18 (3) (generators) -
```

# Example Programs
More in the `good` directory.

## Hello World
```
print "Hello world!";
```

## Recursive Factorial
```
int factr (int n) {
  if (n < 2)
    return 1 ;
  else
    return (n * factr(n-1)) ;
}

print factr(3);
```

## Pass by Value and Reference
```
int valref (int x, int & y) {
   x = x + 1;
   y = y + 1;

   return x + y;
}

int x = 1;
int y = 1;

print valref(x,y);
print x;
print y;
```

## Nested Function Definition
```
string level1() {
  string level2() {
    return "level 2";
  }
  print level2();
  return "level 1";
}

print level1();
```