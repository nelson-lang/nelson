---
name: nelson-usage-linear-algebra-sparse
audience: user
modules: linear_algebra, sparse
description: Solve linear systems and run matrix decompositions in Nelson, dense or sparse. Use when the user solves Ax=b, computes eigenvalues/SVD/QR/LU, norms, ranks, or works with sparse matrices.
triggers: matrice, matrix, lineaire, linear, systeme, system, resoudre, solve, inverse, inv, determinant, det, valeur propre, eigenvalue, eig, svd, qr, lu, cholesky, chol, norme, norm, rang, rank, creuse, sparse, decomposition, kron, produit scalaire, dot, conditionnement, cond
functions: inv, det, eig, svd, qr, lu, chol, norm, rank, trace, pinv, linsolve, kron, cross, dot, null, orth, expm, sqrtm, cond, sparse, full, spdiags, speye, nnz, issparse, eigs, svds
---

# Linear Algebra and Sparse

Solve systems and decompose matrices, using the backslash solver and sparse storage where they pay off.

## When to use
- Solving `A x = b`, inverting, or computing a determinant / rank / norm.
- Decompositions: `eig`, `svd`, `qr`, `lu`, `chol`.
- Large, mostly-zero matrices (sparse).

## When NOT to use
- Plain element-wise arithmetic or statistics (use the analysis guide).

## Key functions
Solve/invert: backslash `A\b`, `linsolve`, `inv`, `pinv`, `det`, `rank`, `cond`, `norm`, `trace`. Decompose: `eig`, `svd`, `qr`, `lu`, `chol`, `null`, `orth`. Functions of a matrix: `expm`, `sqrtm`. Products: `kron`, `cross`, `dot`. Sparse: `sparse`, `full`, `speye`, `spdiags`, `nnz`, `issparse`, and the iterative `eigs` / `svds`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Solve with backslash `x = A\b` - never `inv(A)*b`. It picks a stable method and is faster and more accurate.
2. Keep large, sparse matrices sparse: build with `sparse`/`speye`/`spdiags`; `A\b` stays sparse-aware. Only `full` when a dense result is truly needed.
3. For a few eigenvalues/singular values of a large matrix, use `eigs`/`svds`, not `eig`/`svd` on the full matrix.
4. Check conditioning with `cond` (or a small `rank`) before trusting a near-singular solve.
5. Pick the decomposition that matches structure: `chol` for symmetric positive-definite, `qr` for least squares, `lu` otherwise.

## Gotchas
- `inv` of a singular/ill-conditioned matrix gives garbage without erroring; prefer backslash and check `cond`.
- `eig` returns a vector of eigenvalues with one output, `[V, D]` with two.
- `full()` on a big sparse matrix can exhaust memory.

## Done when
The system is solved or the matrix decomposed with the method suited to its structure, sparsity is preserved where it helps, and conditioning is checked when the solve is near-singular.
