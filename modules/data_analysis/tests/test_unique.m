%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('unique'), -1);
asserts.isequal(nargout('unique'), -1);
%=============================================================================
A = [9 2 9 5 2];
asserts.isequal(unique(A), [2 5 9]);
%=============================================================================
[C, ia, ic] = unique(A, 'stable');
asserts.isequal(C, [9 2 5]);
asserts.isequal(ia, [1; 2; 4]);
asserts.isequal(ic, [1; 2; 1; 3; 2]);
%=============================================================================
A = [2 3; 1 4; 2 3; 0 9];
[C, ia, ic] = unique(A, 'rows', 'stable');
asserts.isequal(C, [2 3; 1 4; 0 9]);
asserts.isequal(ia, [1; 2; 4]);
asserts.isequal(ic, [1; 2; 1; 3]);
%=============================================================================
[uniqueValues, firstIndices, sourceIndices] = unique([9 2 9 5 2], 'stable');
asserts.isequal(uniqueValues, [9 2 5]);
asserts.isequal(firstIndices, [1; 2; 4]);
asserts.isequal(sourceIndices, [1; 2; 1; 3; 2]);
[uniqueRows, rowFirstIndices, rowSourceIndices] = unique([1 2; 1 2; 3 4], 'rows');
asserts.isequal(uniqueRows, [1 2; 3 4]);
asserts.isequal(rowFirstIndices, [1; 3]);
asserts.isequal(rowSourceIndices, [1; 1; 2]);
%=============================================================================
% 'first' / 'last' occurrence options
[u, ia, ic] = unique([3 1 2 1 3], 'first');
asserts.isequal(u, [1 2 3]);
asserts.isequal(ia, [2; 3; 1]);
asserts.isequal(ic, [3; 1; 2; 1; 3]);
%=============================================================================
[u, ia, ic] = unique([3 1 2 1 3], 'last');
asserts.isequal(u, [1 2 3]);
asserts.isequal(ia, [4; 3; 5]);
asserts.isequal(ic, [3; 1; 2; 1; 3]);
%=============================================================================
[u, ia] = unique([1 2; 3 4; 1 2], 'rows', 'last');
asserts.isequal(u, [1 2; 3 4]);
asserts.isequal(ia, [3; 2]);
%=============================================================================
[u, ia, ic] = unique([3 1 2 1 3], 'stable', 'last');
asserts.isequal(u, [2 1 3]);
asserts.isequal(ia, [3; 4; 5]);
asserts.isequal(ic, [3; 2; 1; 2; 3]);
%=============================================================================
% unique(...,'rows') on a matrix with no rows must not crash
R = unique(zeros(0, 3), 'rows');
assert_isequal(size(R), [0 3]);
[C, ia, ic] = unique(zeros(0, 3), 'rows');
assert_isequal(size(C), [0 3]);
assert_isequal(size(ia), [0 1]);
assert_isequal(size(ic), [0 1]);
assert_isequal(size(unique(zeros(0, 2), 'rows', 'stable')), [0 2]);
%=============================================================================
