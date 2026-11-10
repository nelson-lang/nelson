%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 1; 1 2; 1 3];
b = [1; 2; 2];
assert_isapprox(lscov(A, b), [2/3; 0.5], 1e-12);
assert_isapprox(lscov(A, b, [1; 2; 1]), [0.75; 0.5], 1e-12);
assert_isapprox(lscov(A, b, diag([1 2 1])), [0.6; 0.5], 1e-12);
assert_isapprox(lscov(A, b, diag([1 2 1]), 'orth'), [0.6; 0.5], 1e-12);
assert_isapprox(lscov(A, b, diag([1 0 1])), [1; 0.5], 1e-12);
[x, stdx, mse, S] = lscov(A, b);
assert_isapprox(stdx, [0.623609564462324; 0.288675134594813], 1e-12);
assert_isapprox(mse, 1/6, 1e-12);
assert_isapprox(S, [7/18 -1/6; -1/6 1/12], 1e-12);
[x, stdx, mse] = lscov(A, [1 2; 2 3; 2 5]);
assert_isapprox(x, [2/3 1/3; 0.5 1.5], 1e-12);
assert_isapprox(mse, [1/6 1/6], 1e-12);
[x, stdx, mse] = lscov(eye(2), [1; 2]);
assert_isequal(stdx, [0; 0]);
assert_isequal(mse, 0);
w = warning('off', 'Nelson:lscov:rankDeficient');
[x, stdx, mse] = lscov([1 1; 2 2; 3 3], [1; 2; 4]);
warning(w);
assert_isapprox(x, [17/14; 0], 1e-12);
assert_isapprox(stdx, [sqrt(5 / 392); 0], 1e-12);
assert_isapprox(mse, 5 / 28, 1e-12);
assert_checkerror('lscov(A, b, [1; -1; 1])', _('V must be an m-by-m covariance matrix or a vector of m nonnegative weights.'));
assert_checkerror('lscov(A, b, eye(3), ''foo'')', _('Algorithm must be ''chol'' or ''orth''.'));
%=============================================================================
