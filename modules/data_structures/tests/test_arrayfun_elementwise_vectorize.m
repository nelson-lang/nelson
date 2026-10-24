%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Auto-vectorization fast path of arrayfun: when the anonymous callback body is
% a provably element-wise expression, the body is evaluated once on the full
% arrays instead of the per-element loop. Every case below must return the same
% result as the equivalent hand-vectorized expression; the fallback cases must
% preserve the original per-element semantics (especially the matrix-operator
% trap, where `*` `/` `^` must stay element-wise, NOT matrix operations).
%=============================================================================
A = reshape(1:12, 3, 4);
B = reshape(12:-1:1, 3, 4);
%=============================================================================
% Vectorizable bodies must equal the hand-vectorized expression. Transcendental
% functions are compared up to 1 ulp (1e-15 relative): the scalar libm result of
% the per-element path and the SIMD result of the array path can differ by one
% rounding on some CPUs (seen on the Linux CI runner for exp).
asserts.isapprox(arrayfun(@(x, y) sqrt(x .^ 2 + y .^ 2), A, B), sqrt(A .^ 2 + B .^ 2), 1e-15);
asserts.isapprox(arrayfun(@(x) sin(x) + 1, A), sin(A) + 1, 1e-15);
asserts.isapprox(arrayfun(@(x) exp(-x) ./ (1 + abs(x)), A), exp(-A) ./ (1 + abs(A)), 1e-15);
asserts.isapprox(arrayfun(@(x, y) x .^ y, A, B), A .^ B, 1e-15);
asserts.isequal(arrayfun(@(x, y) x .* y - x ./ (y + 1), A, B), A .* B - A ./ (B + 1));
asserts.isequal(arrayfun(@(x) - x, A), -A);
%=============================================================================
% Relational / logical bodies produce logical arrays.
asserts.isequal(arrayfun(@(x, y) x > y, A, B), A > B);
asserts.isequal(arrayfun(@(x, y)(x > 2) & (y < 10), A, B), (A > 2) & (B < 10));
asserts.isequal(arrayfun(@(x, y)(x <= 3) | (y == 12), A, B), (A <= 3) | (B == 12));
%=============================================================================
% Whitelisted element-wise functions (same-size form).
asserts.isequal(arrayfun(@(x, y) min(x, y), A, B), min(A, B));
asserts.isequal(arrayfun(@(x, y) max(x, y), A, B), max(A, B));
asserts.isequal(arrayfun(@(x, y) mod(x, y), A, B), mod(A, B));
asserts.isapprox(arrayfun(@(x, y) hypot(x, y), A, B), hypot(A, B), 1e-15);
asserts.isapprox(arrayfun(@(x, y) atan2(x, y), A, B), atan2(A, B), 1e-15);
%=============================================================================
% Captured scalar is inlined and stays correct.
c = 5;
asserts.isequal(arrayfun(@(x) x + c, A), A + c);
d = 2.5;
asserts.isequal(arrayfun(@(x) x .* d + c, A), A .* d + c);
%=============================================================================
% THE MATRIX-OPERATOR TRAP: inside arrayfun each element is a scalar, so `*`
% `/` `^` mean element-wise; the body is NOT vectorizable (would become matrix
% product / division / power on full arrays) and must fall back.
v = [1 2 3];
w = [4 5 6];
asserts.isequal(arrayfun(@(x, y) x * y, v, w), v .* w);
asserts.isequal(arrayfun(@(x, y) x / y, v, w), v ./ w);
asserts.isequal(arrayfun(@(x, y) x ^ y, [2 3], [3 2]), [8 9]);
asserts.isequal(arrayfun(@(x) x', v), v);
%=============================================================================
% Named function handles to element-wise builtins are vectorized too (arrayfun
% inputs are the same size, so max/min/mod/rem are element-wise and safe).
asserts.isapprox(arrayfun(@sin, A), sin(A), 1e-15);
asserts.isapprox(arrayfun(@exp, A), exp(A), 1e-15);
asserts.isapprox(arrayfun(@sqrt, A), sqrt(A), 1e-15);
asserts.isequal(arrayfun(@abs, -A), abs(-A));
asserts.isequal(arrayfun(@plus, A, B), A + B);
asserts.isequal(arrayfun(@minus, A, B), A - B);
asserts.isequal(arrayfun(@times, A, B), A .* B);
asserts.isequal(arrayfun(@rdivide, A, B), A ./ B);
asserts.isequal(arrayfun(@max, A, B), max(A, B));
asserts.isequal(arrayfun(@min, A, B), min(A, B));
asserts.isequal(arrayfun(@mod, A, B), mod(A, B));
asserts.isapprox(arrayfun(@hypot, A, B), hypot(A, B), 1e-15);
asserts.isequal(arrayfun(@gt, A, B), A > B);
asserts.isequal(arrayfun(@and, A > 2, B > 2), (A > 2) & (B > 2));
asserts.isequal(arrayfun(@xor, A > 5, B > 5), xor(A > 5, B > 5));
%=============================================================================
% Named-handle traps: @max/@min/@sum with a SINGLE argument are reductions, not
% element-wise, and must fall back (per-element reduction of a scalar = scalar).
asserts.isequal(arrayfun(@max, A), A);
asserts.isequal(arrayfun(@min, A), A);
asserts.isequal(arrayfun(@sum, A), A);
asserts.isequal(arrayfun(@prod, A), A);
%=============================================================================
% Non element-wise bodies must fall back and stay correct.
asserts.isequal(arrayfun(@(x) sum(x), A), A); % reduction of a scalar
asserts.isequal(arrayfun(@(x) x(1), A), A); % indexing a scalar
%=============================================================================
% Captured non-scalar array: the per-element path errors on a non-scalar in a
% uniform output; the fast path must NOT silently broadcast instead.
cap = [1 2];
asserts.checkerror('cap = [1 2]; r = arrayfun(@(x) x + cap, reshape(1:12, 3, 4));', ...
  _('Non-scalar in Uniform output.'));
%=============================================================================
% UniformOutput false wraps each scalar result in a cell (no vectorization).
r = arrayfun(@(x) x * [1 1], [1 2], 'UniformOutput', false);
asserts.isequal(r, {[1 1], [2 2]});
%=============================================================================
% Multiple outputs are not vectorized and stay correct.
[q, s] = arrayfun(@(x, y) deal(x + y, x - y), A, B);
asserts.isequal(q, A + B);
asserts.isequal(s, A - B);
%=============================================================================
% Shape preservation across various shapes.
asserts.isequal(arrayfun(@(x) sin(x) + 1, []), []);
asserts.isequal(arrayfun(@(x) x .^ 2, 0.5), 0.25);
T = reshape(1:24, 2, 3, 4);
asserts.isequal(arrayfun(@(x) x .^ 2 + 1, T), T .^ 2 + 1);
R = (1:6)';
asserts.isequal(arrayfun(@(x) 2 .* x, R), 2 .* R);
%=============================================================================
% Type / value preservation.
Ai = int32(A);
asserts.isequal(arrayfun(@(x) x + int32(1), Ai), Ai + int32(1));
Z = A + i * B;
asserts.isequal(arrayfun(@(x) abs(x), Z), abs(Z));
asserts.isequal(arrayfun(@(x) x + 1, true(2, 2)), double(true(2, 2)) + 1);
%=============================================================================
% A body that ignores its argument and returns a scalar must still fill the
% whole output shape (falls back on the shape guard).
asserts.isequal(arrayfun(@(x) 7, A), 7 * ones(size(A)));
%=============================================================================
