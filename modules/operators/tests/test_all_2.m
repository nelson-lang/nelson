%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = all(logical(ones(99, 0)));
assert_isequal(size(A), [1 0]);
A = all(logical(ones(0, 99)));
REF = logical([1:99]);
assert_isequal(A, REF);
A = all(logical(ones(0, 0)));
assert_isequal(A, true);
%=============================================================================
A = all(logical(ones(99, 0)), 1);
assert_isequal(size(A), [1 0]);
A = all(logical(ones(99, 0)), 2);
assert_isequal(A, logical(ones(99, 1)));
A = all(logical(ones(99, 0)), 3);
assert_isequal(size(A), [99 0]);
%=============================================================================
A = all(logical(ones(0, 0)), 1);
assert_isequal(size(A), [1 0]);
A = all(logical(ones(0, 0)), 2);
assert_isequal(size(A), [0 1]);
A = all(logical(ones(0, 0)), 3);
assert_isequal(size(A), [0 0]);
%=============================================================================
A = all(logical(ones(0, 99)), 1);
assert_isequal(A, logical(ones(1, 99)));
A = all(logical(ones(0, 99)), 2);
assert_isequal(size(A), [0 1]);
A = all(logical(ones(0, 99)), 3);
assert_isequal(size(A), [0 99]);
%=============================================================================
A = all(logical(eye(33, 44)));
assert_isequal(A, false(1, 44));
A = all(logical(eye(33, 44)), 1);
assert_isequal(A, false(1, 44));
A = all(logical(eye(33, 44)), 2);
assert_isequal(A, false(33, 1));
A = all(logical(eye(33, 44)), 3);
assert_isequal(A, logical(eye(33, 44)));
%=============================================================================
A = all([0 0 3;0 0 3;0 0 3], 'all');
assert_isequal(A, false);
%=============================================================================
A = all([1 1 1], 'all');
assert_isequal(A, true);
%=============================================================================

