%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(paddata(1:3, 5), [1 2 3 0 0]);
assert_isequal(paddata(1:3, 5, 'FillValue', 9), [1 2 3 9 9]);
assert_isequal(paddata(1:3, 5, 'Side', 'leading'), [0 0 1 2 3]);
assert_isequal(paddata(1:3, 6, 'Side', 'both'), [0 1 2 3 0 0]);
assert_isequal(paddata(1:3, 6, 'Pattern', 'edge'), [1 2 3 3 3 3]);
assert_isequal(paddata(1:3, 8, 'Pattern', 'circular'), [1 2 3 1 2 3 1 2]);
assert_isequal(paddata(1:3, 8, 'Pattern', 'flip'), [1 2 3 3 2 1 1 2]);
assert_isequal(paddata(1:3, 8, 'Pattern', 'reflect'), [1 2 3 2 1 2 3 2]);
assert_isequal(paddata(1:3, 6, 'Side', 'both', 'Pattern', 'edge'), [1 1 2 3 3 3]);
assert_isequal(paddata(1:3, 5, 'Side', 'leading', 'Pattern', 'reflect'), [3 2 1 2 3]);
assert_isequal(paddata([1 2; 3 4], 3), [1 2; 3 4; 0 0]);
assert_isequal(paddata([1 2; 3 4], [3 4]), [1 2 0 0; 3 4 0 0; 0 0 0 0]);
assert_isequal(paddata([1 2; 3 4], 3, 'Dimension', 2), [1 2 0; 3 4 0]);
assert_isequal(paddata(1:5, 3), 1:5);
assert_isequal(class(paddata(single(1:3), 5)), 'single');
assert_isequal(paddata(int8(1:3), 5, 'FillValue', 300), int8([1 2 3 127 127]));
assert_isequal(paddata({1, 2}, 4), {1, 2, [], []});
R = paddata(["a" "b"], 3);
assert_isequal(R(1:2), ["a" "b"]);
assert_istrue(ismissing(R(3)));
assert_isequal(double(paddata('ab', 4)), [97 98 0 0]);
assert_isequal(paddata([1 2], 3, 'FillValue', NaN), [1 2 NaN]);
assert_isequal(size(paddata(ones(2, 2, 2), [3 3 3])), [3 3 3]);
assert_isequal(paddata(true, 3, 'Dimension', 2), [true false false]);
assert_checkerror('paddata(1:3, 5, ''Pattern'', ''zigzag'')', _('Pattern must be ''constant'', ''edge'', ''circular'', ''flip'' or ''reflect''.'));
%=============================================================================
