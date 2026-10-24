%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('sort'), -1);
asserts.isequal(nargout('sort'), 2);
%=============================================================================
[sortedValues, sortedIndices] = sort([3 1 2]);
asserts.isequal(sortedValues, [1 2 3]);
asserts.isequal(sortedIndices, [2 3 1]);
asserts.isequal(sort([3 1 2; 9 7 8], 2, 'descend'), [3 2 1; 9 8 7]);
%=============================================================================
% ComparisonMethod 'abs' on real arrays
assert_isequal(sort([-3 1 -2], 'ComparisonMethod', 'abs'), [1 -2 -3]);
assert_isequal(sort([-2 2 -1 1], 'ComparisonMethod', 'abs'), [1 -1 2 -2]);
[s, i] = sort([-2 2 -1 1], 'ComparisonMethod', 'abs');
assert_isequal(s, [1 -1 2 -2]);
assert_isequal(i, [4 3 2 1]);
assert_isequal(sort([-3 1 -2], 'descend', 'ComparisonMethod', 'abs'), [-3 -2 1]);
assert_isequal(sort([-3 1; 2 -4], 'ComparisonMethod', 'abs'), [2 1; -3 -4]);
assert_isequal(sort(single([-3 1 -2]), 'ComparisonMethod', 'abs'), single([1 -2 -3]));
%=============================================================================
asserts.checkerror('sort([1 2 3], ''bad'')', _('Sort direction must be either the string ''ascend'' or ''descend'''));
asserts.checkerror('sort([1 2 3], ''bad'')', _('Sort direction must be either the string ''ascend'' or ''descend'''), 'Nelson:data_analysis:invalidSortDirection');
%=============================================================================
