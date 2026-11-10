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
X = pagemrdivide(permute(B, [2 1 3]), A);
assert_isequal(size(X), [1 2 2]);
assert_isapprox(X(:), [-0.1; 0.7; 1.1; 0.8], 1e-12);
[X, r] = pagemrdivide([1 2], cat(3, [4 1; 2 3], eye(2)));
assert_isapprox(r(:), [1/3; 1], 1e-12);
assert_checkerror('pagemrdivide(ones(1, 3), ones(2, 2))', _('The two inputs must have the same number of columns.'));
%=============================================================================
