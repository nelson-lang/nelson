%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('any'), 2);
assert_isequal(nargout('any'), 1);
%=============================================================================
A = any(logical(ones(33, 0)));
assert_isequal(size(A), [1 0]);
A = any(logical(ones(0, 99)));
assert_isequal(A, false(1, 99));
A = any(logical(ones(0, 0)));
assert_isequal(A, false);
%=============================================================================
A = any(logical(ones(99, 0)), 1);
assert_isequal(size(A), [1 0]);
A = any(logical(ones(99, 0)), 2);
assert_isequal(A, false(99, 1));
A = any(logical(ones(99, 0)), 3);
assert_isequal(size(A), [99 0]);
%=============================================================================
A = any(logical(ones(0, 0)), 1);
assert_isequal(size(A), [1 0]);
A = any(logical(ones(0, 0)), 2);
assert_isequal(size(A), [0 1]);
A = any(logical(ones(0, 0)), 3);
assert_isequal(size(A), [0 0]);
%=============================================================================
A = any(logical(ones(0, 99)), 1);
assert_isequal(A, false(1, 99));
A = any(logical(ones(0, 99)), 2);
assert_isequal(size(A), [0 1]);
A = any(logical(ones(0, 99)), 3);
assert_isequal(size(A), [0 99]);
%=============================================================================
A = any(logical(eye(3, 4)));
assert_isequal(A, [true(1, 3) false]);
A = any(logical(eye(3, 4)), 1);
assert_isequal(A, [true(1, 3) false]);
A = any(logical(eye(33, 44)), 2);
assert_isequal(A, true(33, 1));
A = any(logical(eye(33, 44)), 3);
assert_isequal(A, logical(eye(33, 44)));
%=============================================================================

