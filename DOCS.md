# Documentation

**yatl** is an imperative, statically typed, lexically scoped and memory-unsafe language.
It supports simple control-flow constructs, recursion, forward declarations of functions,
global variables, handy syntax for multi-dimensional arrays, ternary operators and more.
Sample code can be found in the [`/examples`](/examples) directory.

Every translation unit must have a main function.

## Types

### Basic Data Types

yatl has 12 basic data types: unit (similar to C's void), bool, signed and unsigned integers
(i8, i16, i32, i64, u8, u16, u32 and u64) and floating-point numbers (single precision
float and double precision double). Integers can be written either in decimal, binary (`0b...`),
octal (`0o...`) or hexadecimal (`0x...`) format. Strings are represented as [array](#arrays) of u8: `[-]u8`.

### Relational Operators

Bools and numbers can be compared using `<`, `<=`, `>`, `>=`, `==` or `!=` (`false < true == true`).
Units can be tested only for equality and inequality as well but are always equal.

Relational operators can be chained together: `10 < 12 > 11 == 10 + 1 == true`.

### Unary and Binary Operators

Unary operator `-` changes the sign of a number and `!` performs bitwise negation of a number.
Unary `not` negates a boolean.

Traditional binary arithmetical operators `+`, `-`, `*`, `/` and `%` are supported for numbers as well as
bitshifts `>>`, `<<` and bitwise operators `&`, `^`, `|`, which can be used for booleans as well.

Binary `and` and `or` for booleans. Additionally, `&&` and `||` are their lazy equivalents.

Corresponding assignemnt operators: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `>>=`, `<<=`, `&=`, `|=`, `^=`, `&&=`, `||=`.

### Arrays

Arrays can be allocated either explicitely on stack or on a heap
(see [Buitlin Functions](#builtin-functions) for more information).
yatl also offers neat way to index multidimensional arrays.

```
// Construct one-dimensional array on stack
@[-]u8 array = @[-]u8 [97, 98, 99];

// Arrays can be indexed as usual
u8 result = array[0];
// result == 97

// Construct two-dimensional array on stack
@[-,-]u8 array2D = @[-,-]u8 [
    @[-]u8 [10, 20, 30],
    @[-]u8 [40, 50, 60],
    @[-]u8 [70, 80, 90]
];

u8 result2D = array3D[0,1];
// result2D == 20
```

Pointers are just zero-dimensional arrays, e.g `[]u8`.

### Conversions

yatl performs implicit conversions only when no data loss can occur, e.g. from `u8` to `i16`,
but not from `i64` to `u64`. Arrays cannot be implicitely converted to other arrays.

Programmer can perform explicit type conversions via special syntax `@<to_type>(<expression>)`:

```
i64 signed = 123;
u32 unsigned = @u32(signed);
```

## Control-Flow

yatl offers loops, if statements, ternary operators and function calls as control-flow constructs.

### Loops

```
// Traditional 3-part for-loop separated by `,`
for (i32 i = 0, i < 10, i++) { ... }

// More complex expressions can be used in a for-loop
for (
    i32 i = 0; i32 j = 10,
    i < j,
    i++; j--
    ) { ... }

// Traditional while and do-while loops.
while (true) { ... }
do { ... } while (true)
```

### If Statements, Ternary Operators

```
// Condition must be of type bool.
if (false) { ... } else { ... }

// Else branch can be omitted.
if (true) { ... }
```

```
// ?: is lazily evaluated, first argument must be of type bool.
i32 answer = true ? 30 : 42;
// answer == 30

// ??: is strictly evaluated, first argument must be of type bool.
i32 answerTwo = false ?? 30 : 42;
// answerTwo == 42
```

### Functions

```
u8 add(u8 fst, u8 snd) { return fst + snd; }

// Implict returns are also supported; no `return` statement is needed and the trailing `;` is omitted.
u8 addImplicitReturn(u8 fst, u8 snd) { fst + snd }

i32 main() {
    return @i32(add(10, 32) == addImplicitReturn(32, 10));
}
```

Forward function declaration is also supported, since function declaration must be known when
a function is called. This is needed when mutual recursion is used. Recursive functions which
call just itself do not need to use forward function declaration.

```
// Forward function declaration.
bool isEven(u32 n);

// Forward function declaration with unnamed arguments.
bool isOdd(u32);

bool isEven(u32 n) {
    if (n == 0) { return true; }
    return isOdd(n-1);
}

bool isOdd(u32 n) {
    if (n == 0) { return false; }
    return isEven(n-1);
}
```

## Builtin Functions

yatl provides 10 builtin functions, following the same syntax as type conversions: `@<builtinName>`.

- `@alloc`
- `@malloc`
- `@realloc`
- `@free`
- `@sizeof`
- `@len`
- `@print`
- `@readln`
- `@bitcast`
- `@exit`
