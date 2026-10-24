%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext')), '/private']);
%=============================================================================
C = sparse([0 -2; 3 + 4i 0]);
asserts.isequal(abs(C), sparse([0 2; 5 0]));
asserts.isequal(conj(C), sparse([0 -2; 3 - 4i 0]));
asserts.isequal(real(C), sparse([0 -2; 3 0]));
asserts.isequal(imag(C), sparse([0 0; 4 0]));

A = sparse([0 -1.2; 2.7 0]);
asserts.isequal(ceil(A), sparse([0 -1; 3 0]));
asserts.isequal(fix(A), sparse([0 -1; 2 0]));
asserts.isequal(floor(A), sparse([0 -2; 2 0]));
asserts.isequal(round(A), sparse([0 -1; 3 0]));

asserts.isequal(tril(C), sparse([0 0; 3 + 4i 0]));
asserts.isequal(triu(C), sparse(1, 2, complex(-2, 0), 2, 2));

B = sparse([Inf 0; NaN -Inf]);
asserts.isequal(isfinite(B), [false true; false false]);
asserts.isequal(isinf(B), sparse([true false; false true]));
asserts.isequal(isnan(B), sparse(2, 1, logical(true), 2, 2));

L = sparse(logical([true false; false true]));
asserts.isequal(abs(L), sparse([1 0; 0 1]));
asserts.isequal(isfinite(L), [true true; true true]);
asserts.isequal(isinf(L), sparse(logical([false false; false false])));
asserts.isequal(isnan(L), sparse(logical([false false; false false])));

H = [sparse([1 0; 0 2]), sparse([0; 3])];
asserts.isequal(H, sparse([1 0 0; 0 2 3]));
V = [sparse([1 0]); sparse([0 2])];
asserts.isequal(V, sparse([1 0; 0 2]));
HL = [sparse(logical([true; false])), sparse(logical([false; true]))];
asserts.isequal(HL, sparse(logical([true false; false true])));
VL = [sparse(logical([true false])); sparse(logical([false true]))];
asserts.isequal(VL, sparse(logical([true false; false true])));

E = exp(sparse([1 0; 0 2]));
E_REF = sparse([1; 2; 1; 2], [1; 1; 2; 2], [exp(1); 1; 1; exp(2)], 2, 2);
assert_sparse_isapprox(E, E_REF, 1e-12);

T = sparse([1 0; 0 2]);
assert_sparse_isapprox(sin(T), sparse([1; 2], [1; 2], [sin(1); sin(2)], 2, 2), 1e-12);
assert_sparse_isapprox(asin(T), sparse([1; 2], [1; 2], [asin(1); asin(2 + 0i)], 2, 2), 1e-12);
assert_sparse_isapprox(atan(T), sparse([1; 2], [1; 2], [atan(1); atan(2)], 2, 2), 1e-12);
assert_sparse_isapprox(sinh(T), sparse([1; 2], [1; 2], [sinh(1); sinh(2)], 2, 2), 1e-12);
assert_sparse_isapprox(tan(T), sparse([1; 2], [1; 2], [tan(1); tan(2)], 2, 2), 1e-12);
assert_sparse_isapprox(tanh(T), sparse([1; 2], [1; 2], [tanh(1); tanh(2)], 2, 2), 1e-12);

COS_REF = sparse([1; 2; 1; 2], [1; 1; 2; 2], [cos(1); 1; 1; cos(2)], 2, 2);
assert_sparse_isapprox(cos(T), COS_REF, 1e-12);
COSH_REF = sparse([1; 2; 1; 2], [1; 1; 2; 2], [cosh(1); 1; 1; cosh(2)], 2, 2);
assert_sparse_isapprox(cosh(T), COSH_REF, 1e-12);
ACOS_REF = sparse([1; 2; 1; 2], [1; 1; 2; 2], [acos(1); pi / 2; pi / 2; acos(2 + 0i)], 2, 2);
assert_sparse_isapprox(acos(T), ACOS_REF, 1e-12);

P = sparse([1 + i, -i; i, 2i]);
asserts.isequal(prod(P), sparse([-1 + i, 2]));
asserts.isequal(prod(sparse([true false; true false])), sparse([1 0]));

X = sparse([1 0; 0 2]);
Y = sparse([0 3; 4 0]);
asserts.isequal(X & Y, sparse(logical([false false; false false])));
asserts.isequal(X | Y, sparse(logical([true true; true true])));
asserts.isequal(X & true, sparse(logical([true false; false true])));
asserts.isequal(X | false, logical([true false; false true]));
%=============================================================================
