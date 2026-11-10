%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% comparing a value with itself: NaN never equals itself
a = [NaN 1 Inf];
assert_isequal(a == a, [false true true]);
assert_isequal(a ~= a, [true false false]);
assert_isequal(a >= a, [false true true]);
assert_isequal(a <= a, [false true true]);
s = single(a);
assert_isequal(s == s, [false true true]);
z = complex([NaN 1], [0 1]);
assert_isequal(z == z, [false true]);
assert_isequal(z ~= z, [true false]);
%=============================================================================
% integers, logical and char keep the same-operand result
i = int8([1 2 3]);
assert_isequal(i == i, true(1, 3));
assert_isequal(i ~= i, false(1, 3));
c = 'abc';
assert_isequal(c >= c, true(1, 3));
L = [true false];
assert_isequal(L <= L, true(1, 2));
%=============================================================================
% a value and its transpose share data but not their shape
v = (1:3)';
assert_isequal(v == v', logical(eye(3)));
assert_isequal(v ~= v', ~logical(eye(3)));
assert_isequal(v >= v', logical(tril(ones(3))));
assert_isequal(v <= v', logical(triu(ones(3))));
k = int32([1; 2]);
assert_isequal(k ~= k.', logical([0 1; 1 0]));
w = 'ab';
assert_isequal(w == w', logical(eye(2)));
%=============================================================================
