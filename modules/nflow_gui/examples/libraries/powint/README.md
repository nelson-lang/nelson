# Example: programmatic code generation (powint)

This example shows how a native nflow block generates C and Rust code
**programmatically**, through the pure-C `NfCodegenContext` callbacks, instead
of the declarative JSON template used by the [cumprod](../cumprod/README.md)
example.

The block, Integer Power (`powint`), computes `y = u^n` for a non-negative
integer parameter `n`. The generated code is an **unrolled product of `n`
factors**, and `n` is read at generation time. Because the number of factors
depends on a parameter, a declarative template cannot express it — this is the
case the C-ABI codegen API exists for.

| File          | Purpose                                                      |
| ------------- | ------------------------------------------------------------ |
| `library.json`| Library metadata, block list, phases, geometry, `defaultParams` (`n`) |
| `blocks.js`   | SVG render function for the palette                          |
| `powint.cpp`  | C++ simulation handler + programmatic codegen emitter        |
| `builder.m`   | Build script (Nelson `dlgeneratemake` / `dlmake`)            |
| `powint_demo.nflow` | Ready-to-run demo diagram (constant → powint → workspace)   |

## Building and using

Same workflow as the cumprod example: run `builder` from this directory in
Nelson, then make the library visible to the engine.

```matlab
cd([modulepath('nflow_gui'), '/examples/libraries/powint']);
builder
NFlow.addlibrarypath([modulepath('nflow_gui'), '/examples/libraries']);

% Run or generate code from the bundled demo diagram.
model = [modulepath('nflow_gui'), '/examples/libraries/powint/powint_demo.nflow'];
nflow_codegenerate(model, tempdir(), false, 'C');
```

The shared library is written next to `library.json` as `nflow_powint.dll`
(`.so` / `.dylib` on Linux / macOS).

## How the programmatic codegen works

The plugin exports `nfEmitBlockCodegen(type, lang, ctx)`. The engine calls it
during code generation with a context of pure-C callbacks and routes the
emitted lines into the generated C or Rust. For `powint` the emitter:

1. reads `n` with `ctx->paramNumber(ctx->self, "n", 2.0)`;
2. gets the output lvalue `ctx->outputVar(ctx->self)` and the input expression
   `ctx->inputExpr(ctx->self, 0)`;
3. emits one line, built in a loop of `n` factors, with `ctx->line(...)`.

So `n = 3` on an input wired from block `src` generates (C):

```c
out_<id> = (out_src) * (out_src) * (out_src);
```

and `n = 0` generates `out_<id> = 1.0;`. The same emitter serves Rust: numeric
literals come from `ctx->fmt`, which renders `1.0` in C and `1.0_f64` in Rust.

The block is stateless, so the emitter only emits a step line; a stateful block
would also call `ctx->addState(...)` to declare its state (see the header
`BlockPlugin.h` for the full callback set and the state/step passes).

## When to use which codegen hook

- `nfGetBlockCodegen` (declarative JSON template, see cumprod): simple
  placeholder substitution, no logic.
- `nfEmitBlockCodegen` (this example): the generated code needs real logic —
  loops, parameter-driven branching, per-port decisions.

When a block exports both for the same `(type, lang)`, the programmatic emitter
wins. Neither hook lets any C++ or third-party type cross the plugin boundary.
