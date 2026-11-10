%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
S = sparse([4 1 0 0; 1 5 2 0; 0 2 6 3; 1 0 3 7]);
R = sparse([1 2 0; 0 3 4; 5 0 6; 7 8 0]);
W = sparse([0 2 1; 3 0 1; 1 1 0]);
inputs = {S, R, R', W, S + 1i * speye(4), sparse(5)};
for k = 1:numel(inputs)
  check_four_and_five(inputs{k}, 1e-12);
end
check_four_and_five(sparse(single(full(S))), 1e-5);
check_four_and_five(sparse(single(full(R) + 1i)), 1e-5);
%=============================================================================
% row scaling uses |re| + |im| of each row, and 1 for an empty row
[~, ~, ~, ~, D] = lu(S);
asserts.isequal(full(D), diag([5 8 11 11]));
[~, ~, ~, ~, D] = lu(S + 1i * speye(4));
asserts.isequal(full(D), diag([6 9 12 12]));
[~, ~, ~, ~, D] = lu(sparse([0 0 0; 1 2 0; 0 0 3]));
asserts.isequal(full(D), diag([1 3 3]));
[L, U, P, Q, D] = lu(sparse(single([4 1; 1 5])));
asserts.isequal(class(D), 'single');
asserts.istrue(issparse(D));
%=============================================================================
% threshold partial pivoting keeps the current row when it is large enough
B = sparse([1 0; 2 1]);
[~, ~, p] = lu(B, 0.5, 'vector');
asserts.isequal(p, [1 2]);
[~, ~, p] = lu(B, 0.6, 'vector');
asserts.isequal(p, [2 1]);
[~, ~, p] = lu(B, 1, 'vector');
asserts.isequal(p, [2 1]);
[~, ~, p] = lu(B, single(0.4), 'v');
asserts.isequal(p, [1 2]);
[~, ~, p] = lu(B, sparse(0.4), 'v');
asserts.isequal(p, [1 2]);
%=============================================================================
A = sparse([1 2 5; 4 1 0; 2 3 1]);
[L, U, P] = lu(A, 0.5);
asserts.isequal(full(P), [0 1 0; 1 0 0; 0 0 1]);
asserts.isapprox(full(L), [1 0 0; 1/4 1 0; 1/2 10/7 1], 1e-14);
asserts.isapprox(full(U), [4 1 0; 0 7/4 5; 0 0 -43/7], 1e-14);
[L2, U2] = lu(A, 0.5);
asserts.isapprox(full(L2), full(P' * L), 1e-14);
asserts.isequal(U2, U);
[~, ~, P] = lu(A);
asserts.isequal(full(P), [0 1 0; 0 0 1; 1 0 0]);
%=============================================================================
T = sparse([1 2; 3 4; 5 7; 0 1]);
Y = lu(T, 0.1);
asserts.istrue(issparse(Y));
asserts.isapprox(full(Y), [1 2; 3 -2; 5 1.5; 0 -0.5], 1e-14);
Y = lu(T);
asserts.isapprox(full(Y), [5 7; 0 1; 0.2 0.6; 0.6 -0.2], 1e-14);
%=============================================================================
% a zero pivot column is kept, as a zero on the diagonal of U
[L, U, P] = lu(sparse([1 0 0; 2 0 0; 3 0 1]));
asserts.isapprox(full(L), [1 0 0; 2/3 1 0; 1/3 0 1], 1e-14);
asserts.isapprox(full(U), [3 0 1; 0 0 -2/3; 0 0 -1/3], 1e-14);
asserts.isequal(full(P), [0 0 1; 0 1 0; 1 0 0]);
[L, U, P, Q] = lu(sparse(zeros(2, 2)));
asserts.isequal(nnz(U), 0);
asserts.isequal(full(P' * P), eye(2));
asserts.isequal(full(Q' * Q), eye(2));
%=============================================================================
% empty inputs
[L, U, P, Q, D] = lu(sparse(zeros(3, 0)));
asserts.isequal(size(L), [3 0]);
asserts.isequal(size(U), [0 0]);
asserts.isequal(full(P), eye(3));
asserts.isequal(size(Q), [0 0]);
asserts.isequal(full(D), eye(3));
[~, ~, p, q] = lu(sparse(zeros(3, 0)), 'vector');
asserts.isequal(p, [1 2 3]);
asserts.isequal(size(q), [1 0]);
%=============================================================================
% a column order keeps the factors sparse
n = 30;
e = ones(n, 1);
T1 = spdiags([-e, 2 * e, -e], -1:1, n, n);
K = kron(speye(n), T1) + kron(T1, speye(n));
[L, U, P, Q] = lu(K);
asserts.istrue(norm(P * K * Q - L * U, 1) < 1e-12);
fill4 = nnz(L) + nnz(U);
[L, U, P] = lu(K);
fill3 = nnz(L) + nnz(U);
asserts.istrue(fill4 < fill3 / 2);
[L, U, P, Q] = lu(K, [0.5, 0.01]);
asserts.istrue(norm(P * K * Q - L * U, 1) < 1e-12);
[L, U, P, Q] = lu(K, 1);
asserts.istrue(norm(P * K * Q - L * U, 1) < 1e-12);
%=============================================================================
msg = _('The pivot threshold must be between 0 and 1.');
assert_checkerror('lu(S, 2)', msg, 'Nelson:lu:pivotThreshold');
assert_checkerror('lu(S, -1)', msg, 'Nelson:lu:pivotThreshold');
assert_checkerror('lu(S, NaN)', msg, 'Nelson:lu:pivotThreshold');
assert_checkerror('lu(S, 0.5i)', msg, 'Nelson:lu:pivotThreshold');
msg = _('The pivot threshold must be a scalar with three outputs or fewer.');
assert_checkerror('lu(S, [0.1 0.2])', msg, 'Nelson:lu:nonScalarPivotThreshold');
assert_checkerror('[L, U, P] = lu(S, [])', msg, 'Nelson:lu:nonScalarPivotThreshold');
msg = _('The pivot threshold must be a scalar or a vector of two elements.');
assert_checkerror('[L, U, P, Q] = lu(S, [0.1 0.2 0.3])', msg, 'Nelson:lu:pivotThresholdSize');
assert_checkerror('[L, U, P, Q] = lu(S, [])', msg, 'Nelson:lu:pivotThresholdSize');
msg = _('The pivot threshold must be double or single.');
assert_checkerror('lu(S, int8(0))', msg, 'Nelson:lu:pivotThresholdType');
msg = _('The output form must be ''matrix'' or ''vector''.');
assert_checkerror('lu(S, true)', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(S, ''vector'', 0.5)', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(S, 0.5, ''foo'')', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(magic(3), 0.5)', msg, 'Nelson:lu:invalidOption');
msg = _('Four or five outputs need a sparse matrix.');
assert_checkerror('[a, b, c, d] = lu(magic(3))', msg, 'Nelson:lu:sparseOnlyOutputs');
assert_checkerror('lu(magic(3), 0.5, ''vector'')', _('Too many input arguments.'));
assert_checkerror('[a, b, c, d, e, f] = lu(S)', _('Too many output arguments.'));
%=============================================================================
function check_four_and_five(A, tol)
  [m, n] = size(A);
  k = min(m, n);
  realClass = class(real(full(A(1:0))));
  [L, U, P, Q] = lu(A);
  asserts.isequal(size(L), [m, k]);
  asserts.isequal(size(U), [k, n]);
  asserts.isequal(size(P), [m, m]);
  asserts.isequal(size(Q), [n, n]);
  asserts.istrue(issparse(L) && issparse(U) && issparse(P) && issparse(Q));
  asserts.isequal(class(L), realClass);
  asserts.isequal(class(P), realClass);
  asserts.isequal(class(Q), realClass);
  asserts.istrue(isreal(P) && isreal(Q));
  asserts.istrue(istril(L) && istriu(U));
  asserts.isequal(full(diag(double(L))), ones(k, 1));
  asserts.isequal(full(double(P) * double(P)'), eye(m));
  asserts.isequal(full(double(Q)' * double(Q)), eye(n));
  asserts.isapprox(full(double(P) * double(A) * double(Q)), full(double(L) * double(U)), tol);
  [L, U, P, Q, D] = lu(A);
  asserts.isequal(size(D), [m, m]);
  asserts.istrue(issparse(D) && isreal(D));
  asserts.isequal(class(D), realClass);
  DA = full(double(D)) \ full(double(A));
  asserts.isapprox(full(double(P)) * DA * full(double(Q)), full(double(L) * double(U)), tol);
  [L, U, p, q] = lu(A, 'vector');
  asserts.isequal(size(p), [1, m]);
  asserts.isequal(size(q), [1, n]);
  asserts.isequal(class(p), 'double');
  asserts.isequal(class(q), 'double');
  asserts.isfalse(issparse(p) || issparse(q));
  asserts.isequal(sort(p), 1:m);
  asserts.isequal(sort(q), 1:n);
  asserts.isapprox(full(double(A(p, q))), full(double(L) * double(U)), tol);
  [~, ~, p, q, D] = lu(A, 'vector');
  asserts.istrue(issparse(D));
end
%=============================================================================
