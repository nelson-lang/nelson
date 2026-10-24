%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected values are listed explicitly below.
%=============================================================================
X = cat(3, [1 2; 3 4], [5 6; 7 8]);
%=============================================================================
% the default 2-norm is the largest singular value of each page
Y = pagenorm(X);
assert_isequal(size(Y), [1 1 2]);
assert_isapprox(Y(:, :, 1), norm([1 2; 3 4]), 1e-12);
assert_isapprox(Y(:, :, 2), norm([5 6; 7 8]), 1e-12);
%=============================================================================
% the 1-norm is the largest column sum of each page
P = pagenorm(X, 1);
assert_isequal(P(:, :, 1), 6);
assert_isequal(P(:, :, 2), 14);
%=============================================================================
% the infinity norm is the largest row sum of each page
P = pagenorm(X, Inf);
assert_isequal(P(:, :, 1), 7);
assert_isequal(P(:, :, 2), 15);
%=============================================================================
% the Frobenius norm of each page
P = pagenorm(X, 'fro');
assert_isapprox(P(:, :, 1), sqrt(1 + 4 + 9 + 16), 1e-12);
assert_isapprox(P(:, :, 2), sqrt(25 + 36 + 49 + 64), 1e-12);
%=============================================================================
% higher dimensional stacks keep their trailing page dimensions
assert_isequal(size(pagenorm(reshape(1:24, 2, 3, 2, 2))), [1 1 2 2]);
%=============================================================================
% a single matrix page returns a scalar
assert_isequal(size(pagenorm([1 2; 3 4])), [1 1]);
%=============================================================================
% the class of the input is preserved
assert_isequal(class(pagenorm(single(X))), 'single');
%=============================================================================
% vector pages use the vector norm
V = cat(3, [1 2 3], [4 5 6]);
P = pagenorm(V, 1);
assert_isequal(P(:, :, 1), 6);
P = pagenorm(V, Inf);
assert_isequal(P(:, :, 1), 3);
P = pagenorm(V, 2);
assert_isapprox(P(:, :, 1), sqrt(14), 1e-12);
%=============================================================================
% non-square pages with the default 2-norm
Z = cat(3, [1 2 3; 4 5 6], [7 8 9; 1 2 3]);
Q = pagenorm(Z);
assert_isapprox(Q(:, :, 1), norm([1 2 3; 4 5 6]), 1e-12);
assert_isapprox(Q(:, :, 2), norm([7 8 9; 1 2 3]), 1e-12);
%=============================================================================
