---
name: nelson-usage-language-basics
audience: user
description: Core Nelson language idioms and conventions for answering everyday usage questions (syntax, indexing, strings, control flow, how to discover functions). Use for general "how do I ... in Nelson" questions that are not about a specific toolbox. Does not list functions; exact signatures come from the Verified Nelson API block.
triggers: how do i, comment faire, comment, syntax, syntaxe, index, indice, matrix, matrice, vector, vecteur, string, chaine, chaîne, char, cell, struct, structure, loop, boucle, for, while, if, function, fonction, script, plot, trace, tracer, figure, graph, graphe, graphique, error, erreur, help, aide, doc, variable, workspace, comment, print, afficher, display
---

# Nelson Language Basics (usage)

Answer the user's Nelson question directly. Use these conventions; for the exact
signature of any function, rely on the "Verified Nelson API" block in the prompt
(do not guess arguments). Do not reference source-tree files: only functions,
`help`, and `doc`, which exist in the running build.

## Conventions

- Arrays and strings are **1-based**: `v(1)` is the first element, `v(end)` the
  last. `end` inside an index means the last element; `:` alone means "all".
- Comments start with `%`. Statements end with `;` to suppress echo; without `;`
  the value is displayed. `...` continues a statement on the next line.
- Two string kinds: single quotes `'abc'` make a **character array**; double
  quotes `"abc"` make a **string**. Double a quote to escape it inside the same
  kind (`'it''s'`).
- Ranges: `1:5` is `[1 2 3 4 5]`; `0:0.1:1` sets the step. Matrices use `[ ]`
  with spaces/commas between columns and `;` between rows: `[1 2; 3 4]`.
- Element-wise operators have a dot: `.*`, `./`, `.^` (vs matrix `*`, `/`, `^`).
- Indexing uses parentheses `A(i, j)`; cells use braces `c{k}`; struct fields use
  a dot `s.field`.

## Control flow (shape, not exhaustive)

```nelson
for k = 1:n
  % ...
end

if cond
  % ...
elseif other
  % ...
else
  % ...
end

function y = f(x)
  y = x .^ 2;
end
```

## Discovering functions

- `help name` prints a function's help; `doc name` opens full documentation.
- When unsure a function exists, say so rather than inventing one. Prefer the
  names shown in the Verified Nelson API block; if a needed function is not
  listed and you are not certain of it, tell the user to check `help`/`doc`.

## Answering style

- Give a short, runnable Nelson snippet when code answers the question, then one
  or two lines of explanation. Keep examples minimal and self-contained.
- Never present MATLAB or Simulink product or function identifiers; use Nelson's
  own names. When a behavior differs from what MATLAB users expect, state the
  Nelson behavior plainly without naming the other product.
