%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = cat(3, [4 1; 2 3], [2 0; 1 5]);
B = cat(3, [1; 2], [3; 4]);
X = pagemldivide(A, B);
assert_isequal(size(X), [2 1 2]);
assert_isapprox(X(:), [0.1; 0.6; 1.5; 0.5], 1e-12);
X = pagemldivide(A, [1; 2]);
assert_isapprox(X(:), [0.1; 0.6; 0.5; 0.3], 1e-12);
assert_isequal(class(pagemldivide(single(A), B)), 'single');
X = pagemldivide(cat(3, [1 2; 3 4; 5 6], [1 0; 0 1; 1 1]), cat(3, [1; 2; 3], [1; 1; 1]));
assert_isapprox(X(:), [0; 0.5; 2/3; 2/3], 1e-12);
[X, r] = pagemldivide(cat(3, [4 1; 2 3], eye(2)), ones(2, 1));
assert_isequal(size(r), [1 1 2]);
assert_isapprox(r(:), [1/3; 1], 1e-12);
X = pagemldivide(ones(2, 2, 1, 3) + reshape(1:12, 2, 2, 1, 3), ones(2, 1, 2));
assert_isequal(size(X), [2 1 2 3]);
assert_checkerror('pagemldivide(int8(eye(2)), [1; 2])', _('Inputs must be single or double arrays.'));
assert_checkerror('pagemldivide(ones(2, 2, 2), ones(3, 1))', _('The two inputs must have the same number of rows.'));
assert_checkerror('pagemldivide(ones(2, 2, 2), ones(2, 1, 3))', _('Dimensions beyond the second must match or be 1.'));
%=============================================================================
