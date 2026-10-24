%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% repmat: replication factors are real finite integers (negative counts as 0)
assert_checkerror('repmat(1:3, 1.5)', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, NaN)', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, Inf)', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, -Inf)', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, 1, NaN)', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, [1 1.5])', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, zeros(1, 0))', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, [])', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, [2; 1])', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, 2, [2 1])', _('Replication factors must be a row vector of integers or integer scalars.'));
assert_checkerror('repmat(1:3, ''a'')', _('Replication factors must be numeric.'));
assert_checkerror('repmat(1:3, {2})', _('Replication factors must be numeric.'));
assert_checkerror('repmat(1:3, 1+1i)', _('Replication factors must be numeric.'));
asserts.isequal(repmat(1:3, -1), zeros(0, 0));
asserts.isequal(repmat(1:3, [2 -1]), zeros(2, 0));
asserts.isequal(repmat(1:3, true), 1:3);
asserts.isequal(repmat(1:3, false), zeros(0, 0));
asserts.isequal(repmat(1:3, int8(2)), [1 2 3 1 2 3; 1 2 3 1 2 3]);
%=============================================================================
% circshift: a shift along a dimension beyond ndims does nothing
asserts.isequal(circshift(1:5, 1, 3), 1:5);
asserts.isequal(circshift(1:5, [1 1 1]), [5 1 2 3 4]);
asserts.isequal(circshift(magic(3), [1 1 1]), reshape([2 6 7 4 8 3 9 1 5], 3, 3));
asserts.isequal(circshift(magic(3), 1, 3), magic(3));
asserts.isequal(circshift(magic(3), [0 0 1]), magic(3));
asserts.isequal(circshift(reshape(1:24, 2, 3, 4), [1 2 3 4]), reshape([10 9 12 11 8 7 16 15 18 17 14 13 22 21 24 23 20 19 4 3 6 5 2 1], 2, 3, 4));
asserts.isequal(circshift(reshape(1:24, 2, 3, 4), 1, 4), reshape(1:24, 2, 3, 4));
asserts.isequal(circshift(5, [1 2 3]), 5);
assert_checkerror('circshift(1:5, 1, 0)', _('DIM must be a positive integer scalar.'));
assert_checkerror('circshift(1:5, 1, -1)', _('DIM must be a positive integer scalar.'));
assert_checkerror('circshift(1:5, true)', _('All values of N must be integer values.'));
%=============================================================================
% flip: the dimension is a positive integer
assert_checkerror('flip(magic(3), 1.5)', _('Dimension argument must be a positive integer scalar.'));
assert_checkerror('flip(magic(3), ''a'')', _('Dimension argument must be a positive integer scalar.'));
assert_checkerror('flip(magic(3), NaN)', _('Dimension argument must be a positive integer scalar.'));
assert_checkerror('flip(magic(3), Inf)', _('Dimension argument must be a positive integer scalar.'));
asserts.isequal(flip(magic(3), true), reshape([4 3 8 9 5 1 2 7 6], 3, 3));
%=============================================================================
% size with several dimension arguments
x = zeros(2, 3, 4);
asserts.isequal(size(x, [1; 2]), [2 3]);
asserts.isequal(size(x, [1 2]'), [2 3]);
asserts.isequal(size(x, []), zeros(1, 0));
asserts.isequal(size(x, 1, 2), [2 3]);
asserts.isequal(size(x, 1, 3), [2 4]);
asserts.isequal(size(x, 3, 1), [4 2]);
asserts.isequal(size(x, 1, 2, 3), [2 3 4]);
asserts.isequal(size(x, 1, 2, 3, 4), [2 3 4 1]);
asserts.isequal(size(1, 2, 3, 4, 5), [1 1 1 1]);
[a, b] = size(x, [1 2]);
asserts.isequal([a b], [2 3]);
[a, b] = size(x, 1, 2);
asserts.isequal([a b], [2 3]);
[a] = size(x, 1, 2);
asserts.isequal(a, [2 3]);
[a, b] = size(x, [4 5]);
asserts.isequal([a b], [1 1]);
assert_checkerror('size(zeros(2, 3, 4), [1 0])', _('Dimension argument must be a positive integer scalar within indexing range.'));
assert_checkerror('[a, b] = size(zeros(2, 3, 4), [1 2 3])', _('Number of outputs must match the number of queried dimensions.'));
assert_checkerror('[a, b, c] = size(zeros(2, 3, 4), [1 2])', _('Too many output arguments.'));
%=============================================================================
% reshape: an unknown dimension of an empty result is 0
asserts.isequal(size(reshape([], 0, [])), [0 0]);
asserts.isequal(size(reshape([], [], 0)), [0 0]);
asserts.isequal(size(reshape(zeros(0, 3), 0, [])), [0 0]);
asserts.isequal(size(reshape(zeros(0, 3), [], 0)), [0 0]);
%=============================================================================
% magic: a finite real scalar
assert_checkerror('magic([2 3])', _('Input must be a finite real scalar.'));
assert_checkerror('magic(NaN)', _('Input must be a finite real scalar.'));
assert_checkerror('magic(Inf)', _('Input must be a finite real scalar.'));
asserts.isequal(magic(true), 1);
asserts.isequal(size(magic('a')), [97 97]);
%=============================================================================
