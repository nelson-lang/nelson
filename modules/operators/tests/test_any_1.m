%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('any'), 2)
assert_isequal(nargout('any'), 1)
%=============================================================================
assert_isequal(any([]), false)
assert_isequal(any([], 1), logical(zeros(1, 0)))
assert_isequal(any([], 2), logical(zeros(0, 1)))
assert_isequal(any([], 3), logical(zeros(0, 0)))
%=============================================================================
assert_isequal(any(ones(1, 0)), false)
assert_isequal(any(ones(0, 1)), false)
%=============================================================================
assert_isequal(any(ones(0, 0, 0)), logical(zeros(1, 0, 0)))
%=============================================================================
assert_isequal(any(ones(0, 2)), [false false])
assert_isequal(any(ones(3,0)), logical(ones(1, 0)))
assert_isequal(any(ones(3, 0, 1)), logical(ones(1, 0)))
%=============================================================================
assert_isequal(any([0 0 3;0 0 3;0 0 3]), logical([0   0   1]))
%=============================================================================
assert_isequal(any([0 0 3;0 0 3;0 0 3], 2), logical([1;   1;   1]))
%=============================================================================
