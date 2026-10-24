%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% with fewer outputs than dimensions, the last output is the product of the
% remaining dimensions
[r, c] = size(zeros(2, 3, 4));
assert_isequal([r, c], [2 12]);
[a, b, c] = size(zeros(2, 3, 4, 5));
assert_isequal([a, b, c], [2 3 20]);
[r, c] = size(zeros(2, 0, 4));
assert_isequal([r, c], [2 0]);
[r, c] = size(zeros(2, 3, 0));
assert_isequal([r, c], [2 0]);
[r, c] = size(zeros(2, 3));
assert_isequal([r, c], [2 3]);
[r, c] = size('NELSON');
assert_isequal([r, c], [1 6]);
[r, c] = size({1, 2; 3, 4; 5, 6});
assert_isequal([r, c], [3 2]);
[r, c] = size(sparse(eye(3)));
assert_isequal([r, c], [3 3]);
%=============================================================================
% more outputs than dimensions are padded with ones
[a, b, c, d] = size(zeros(2, 3));
assert_isequal([a, b, c, d], [2 3 1 1]);
[a, b, c, d] = size(zeros(2, 3, 4));
assert_isequal([a, b, c, d], [2 3 4 1]);
%=============================================================================
% one output keeps every dimension
assert_isequal(size(zeros(2, 3, 4)), [2 3 4]);
r = size(zeros(2, 3, 4));
assert_isequal(r, [2 3 4]);
%=============================================================================
