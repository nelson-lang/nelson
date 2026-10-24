# Intentionally invalid nflow models

These `.nflow` files are **deliberately broken**. They exist so the usability
test suites can assert that nflow surfaces a clear, localized diagnostic to the
user instead of failing silently or crashing.

Do **not** "fix" them and do **not** add them to the Example Browser gallery or
to any round-trip / demo suite: they are fixtures for the negative path.

| File | Defect | Expected diagnostic |
|------|--------|---------------------|
| `algebraic_loop.nflow` | `sum -> gain -> sum` feedback with no state block (direct feedthrough) | An algebraic-loop error is reported when the model is run. |
| `unconnected_input.nflow` | `gain` block whose input port is left dangling | An unconnected-input error/warning is reported for the offending block. |

Consumed by:
- `tests/b2b/tests/model-diagnostics.spec.ts` (real Nelson engine)
- `tests/e2e/tests/model-diagnostics.spec.ts` loads them to check they render.
