<link rel="stylesheet" href="highlight.css"/>
<script src="highlight.pack.js"></script>
<script>hljs.highlightAll();</script>

# Getting started with Nelson

Welcome to Nelson: a high-level, numerical computing language inspired by MATLAB(c)/Octave and designed for fast, productive engineering and scientific work.

This guide gives you a quick path from installation to running your first commands, writing scripts, plotting, and finding help. It's intended for new users who want a working quickstart and a few ready-to-run examples.

This tutorial is organized as short lessons you can work through at the REPL or by running small script files.

---

## Tutorial lessons

### Introduction

Nelson is a high-level numerical computing language whose core data type is the array (vectors and matrices). Like MATLAB, Nelson focuses on interactive computing, visualization, and rapid prototyping. This tutorial helps you learn the essentials: REPL usage, basic math, plotting, arrays, scripts, and simple programming constructs.

### Basic features

- Interactive REPL with history and help
- Native arrays (vectors, matrices, N‑D arrays) without explicit dimensioning
- Built-in math and matrix functions (linear algebra, fft, statistics)
- High-level plotting API for quick graphs
- Modules and an extensible module manager (`nmm`)
- Foreign Function Interface (FFI), Python/Julia interop, and workspace I/O (`.nh5`, `.mat`)

### A minimum Nelson session

This quick session shows how to start, perform simple calculations, and quit.

#### Starting Nelson

Open a terminal (PowerShell on Windows, bash/zsh on Unix) and run:

```bash
nelson
```

You will be presented with a prompt. This prompt accepts Nelson expressions.

#### Using Nelson as a calculator

At the prompt try:

```matlab
a = 1 + 2 * 3
b = sin(pi/4)
```

If you don't assign a variable, results are stored in the default variable `ans`.

#### Quitting Nelson

Type:

```matlab
quit
```

or use the REPL shortcut for exiting.

### Getting started: variables and workspace

#### Creating variables

Variables are created with an assignment:

```matlab
x = 2*pi
v = [1, 2, 3]
```

Nelson uses square brackets for vectors and semicolons to separate rows in matrices:

```matlab
A = [1 2; 3 4]
```

#### Overwriting variables and suppressing output

Reassigning is allowed. To suppress immediate printing, terminate a statement with `;`:

```matlab
t = 5;
```

#### Error messages and corrections

Typos produce an error — use the up-arrow to recall previous commands, edit them and re-run.

#### Operator precedence

Use parentheses to control precedence:

```matlab
(1+2)*3    % yields 9
1+2*3      % yields 7
```

#### Managing the workspace

Useful commands:

- `clear` — remove all variables
- `who` — list variable names
- `whos` — detailed list (size, type)
- `save('s.nh5')` — save workspace (HDF5 `.nh5` default)
- `load('s.nh5')` — restore workspace

#### Keeping a session diary

Record all input/output with:

```matlab
diary('session.txt')
diary off
```

---

## Mathematical functions and plotting

### Elementary math functions

Nelson provides a rich set of mathematical functions: `sin`, `cos`, `tan`, `exp`, `log`, `sqrt`, `abs`, and more. Constants such as `pi`, `Inf`, and `NaN` are available.

Examples:

```matlab
y = exp(-5)*sin(2) + 10*sqrt(8)
log(142)
sin(pi/4)
```

### Basic plotting

To plot data, prepare `x` and `y` vectors and call `plot`:

```matlab
x = linspace(0, 2*pi, 201)
y = sin(x)
plot(x, y)
xlabel('x')
ylabel('sin(x)')
title('Sine')
```

Multiple data sets:

```matlab
plot(x, 2*cos(x), '--', x, cos(x), '-', x, 0.5*cos(x), ':')
legend('2*cos(x)', 'cos(x)', '0.5*cos(x)')
```

Line styles, colors and markers follow common short codes: `r`, `b`, `k`, `--`, `:`, `o`, `*`, etc.

---

## Arrays, matrices and linear equations

### Creating vectors and matrices

Row vector:

```matlab
v = [1 4 7 10]
```

Column vector:

```matlab
w = [1; 4; 7; 10]
```

Transpose:

```matlab
w = v'
```

Indexing and submatrices:

```matlab
v(1:3)      % elements 1 to 3
A(2,:)      % second row
A(:,2:3)    % columns 2 and 3
```

Colon operator and `linspace`:

```matlab
0:0.1:5
linspace(0, 2*pi, 101)
```

### Array vs matrix operations

- Matrix multiplication: `A * B`
- Element-wise multiplication: `A .* B`

Use `.` prefix for element-wise operators: `.*`, `./`, `.^` when needed.

### Solving linear systems

Solve `Ax = b` with the backslash operator (preferred for numerical stability):

```matlab
x = A \ b
```

You can also compute `inv(A)*b` but using `A\b` is generally recommended.

---

## Introduction to programming in Nelson (scripts & functions)

### Scripts

Create script files (e.g., `example1.m`) containing a sequence of commands. Run them with:

```bash
nelson -f example1.m
```

Simple script example (save as `example1.m`):

```matlab
% example1.m
A = [1 2 3; 3 3 4; 2 3 3];
b = [1; 1; 2];
x = A \ b
```

Variables created in a script are placed in the global workspace — be aware of side-effects.

### Functions

Functions have their own local workspace and avoid polluting the global scope. Example function file `fact.m`:

```matlab
function f = factorial(n)
    % FACTORIAL(n) Compute factorial using prod
    f = prod(1:n);
end
```

Call with `factorial(5)` to get `120`.

### Input and output

Prompt for input inside a script using `input(...)` and format output with `printf`/`disp` equivalents available in Nelson.

---

## Control flow and operators

### If / for / while

If structure:

```matlab
if expr
    statements
elseif expr2
    statements
else
    statements
end
```

For loop:

```matlab
for i = 1:5
    s = i*i
end
```

While loop:

```matlab
while x <= 10
    x = 3*x
end
```

### Relational and logical operators

Comparison: `>`, `<`, `>=`, `<=`, `==`, `~=`  
Logical: `&`, `|`, `~` (element-wise) and `&&`, `||` (short-circuit where supported)

---

## Appendix: Summary of useful commands

This short list collects the most frequently used commands.

- General

  - `quit` — exit Nelson
  - `doc <command>` — show help for a command

- Workspace and files

  - `clear`, `who`, `whos`, `save('file.nh5')`, `load('file.nh5')`, `diary('session.txt')`

- Arrays and matrices

  - `:` colon operator, `linspace(a,b,n)`, `zeros(m,n)`, `ones(m,n)`, `eye(n)`

- Linear algebra

  - `A\b` solve, `inv(A)`, `det(A)`, `eig(A)`, `rank(A)`

- Plotting
  - `plot(x,y)`, `xlabel()`, `ylabel()`, `legend()`, `title()`, `axis()`

---

Happy computing with Nelson!
