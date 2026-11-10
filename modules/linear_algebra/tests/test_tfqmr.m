%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
n = 20;
e = ones(n, 1);
A = spdiags([-1 * e, 2.5 * e, -0.5 * e], -1:1, n, n);
b = (1:n).';
[x, flag, relres, iter, resvec] = tfqmr(A, b, 1e-8, 30);
assert_isequal(flag, 0);
assert_isequal(iter, 10);
assert_isapprox(relres, 8.0869634227710413e-09, 1e-6);
assert_isequal(size(resvec), [21 1]);
assert_isapprox(resvec(1:6), [53.572380943915498; 8.4954084316855667; 2.9370854644783915; 2.3934763685261609; 0.60804989361366302; 0.20373781269474933], 1e-12);
assert_isapprox(x, A \ b, 1e-6);
[x, flag, relres, iter, resvec] = tfqmr(A, b, 1e-12, 3);
assert_isequal(flag, 1);
assert_isequal(iter, 3);
assert_isapprox(relres, 0.0010950533971119491, 1e-10);
assert_isapprox(x(1:3), [0.7178730712170236; 1.5940280573190773; 2.5388665561659969], 1e-10);
M = spdiags(2.5 * e, 0, n, n);
[x, flag, relres, iter] = tfqmr(A, b, 1e-8, 30, M);
assert_isequal([flag, iter], [0, 10]);
[x, flag, relres, iter] = tfqmr(A, b, 1e-8, 30, [], [], ones(n, 1));
assert_isequal([flag, iter], [0, 13]);
[x, flag, relres, iter, resvec] = tfqmr(A, zeros(n, 1));
assert_isequal([flag, relres, iter, numel(resvec), norm(x)], [0 0 0 1 0]);
[x, flag] = tfqmr(@(v) A * v, b, 1e-8, 30);
assert_isequal(flag, 0);
assert_isapprox(x, A \ b, 1e-6);
x = tfqmr(full(A), b, 1e-8, 30);
assert_isapprox(x, A \ b, 1e-6);
x = tfqmr(A, b, 1e-8, 30, @(v) v / 2.5);
assert_isapprox(x, A \ b, 1e-6);
[x, flag, relres, iter] = tfqmr(A, b);
assert_isequal([flag, iter], [0, 7]);
assert_isapprox(relres, 4.03271e-07, 1e-4);
x = tfqmr(single(full(A)), single(b), 1e-5, 30);
assert_isequal(class(x), 'single');
assert_checkerror('tfqmr(ones(2, 3), [1; 2])', _('Square matrix expected.'));
assert_checkerror('tfqmr(A, b, 1e-8, 30, [], [], ones(3, 1))', _('Initial guess dimensions must agree.'));
%=============================================================================
