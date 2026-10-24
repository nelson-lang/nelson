%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% truncating a column vector to a shorter size keeps the leading elements
y = resize((1:5)', [3 1]);
assert_isequal(y, [1; 2; 3]);
%=============================================================================
% a scalar size resizes the length of a vector; padding uses zero by default
assert_isequal(resize([1 2 3], 6), [1 2 3 0 0 0]);
assert_isequal(resize([1 2 3 4], 2), [1 2]);
%=============================================================================
% a column vector keeps its orientation under a scalar size
assert_isequal(resize([1; 2; 3], 5), [1; 2; 3; 0; 0]);
%=============================================================================
% a matrix is resized per dimension (truncate and pad)
assert_isequal(resize([1 2; 3 4], [2 2]), [1 2; 3 4]);
assert_isequal(resize([1 2; 3 4], [3 3]), [1 2 0; 3 4 0; 0 0 0]);
%=============================================================================
% a custom fill value is used for padded elements
assert_isequal(resize([1 2; 3 4], [3 3], 'FillValue', 9), [1 2 9; 3 4 9; 9 9 9]);
%=============================================================================
% the resized array keeps the class of the input
r = resize(int8([1 2 3]), 5);
assert_isequal(class(r), 'int8');
assert_isequal(r, int8([1 2 3 0 0]));
%=============================================================================
% an invalid size is rejected
assert_checkerror('resize([1 2 3], -1)', _('The size must be a vector of nonnegative integers.'));
%=============================================================================
