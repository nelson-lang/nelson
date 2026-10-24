%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
asserts.isequal(isequal(NaN, NaN), false);
asserts.isequal(isequal(NaN, Inf), false);
asserts.isequal(isequal(NaN, 1.0), false);
asserts.isequal(isequal([11, 12, NaN, 14], [11, 12, NaN, 14]), false);
asserts.isequal(isequal(struct('nelson', NaN, 'nel', 2), struct('nelson', NaN, 'nel', 2), struct('nelson', NaN, 'nel', 2)), false);
%=============================================================================
asserts.isequal(isequal(11, 11, 11, 11, 11), true);
asserts.isequal(isequal(11, 11, 41, 21, 11), false);
asserts.isequal(isequal(0i + 11, 11, 11, 11), true);
asserts.isequal(isequal(0i, 0, 0, 0i), true);
asserts.isequal(isequal(4i - 10, 4i - 10), true);
asserts.isequal(isequal([11, 41, 21; 51, 11, 21], [11, 41, 21; 51, 11, 21], [11, 41, 21; 51, 11, 21]), true);
asserts.isequal(isequal([11, 41, 21; 51, 11, 21], [11, 41, 21; 51, 11, 21], [51, 11, 21; 11, 41, 21]), false);
asserts.isequal(isequal([21 - i, 81, i, 21 + i], [21 - i, 81, i, 21 + i], [21 - i, 81, i, 21 + i], [21 - i, 81, i, 21 + i], [21 - i, 81, i, 21 + i]), true);
asserts.isequal(isequal('test_nelson', 'test_nelson', 'test_nelson'), true);
asserts.isequal(isequal('test_nelson', 'test_nelson', 'test_nelson2'), false);
asserts.isequal(isequal(11, 31, [21, 11]), false);
asserts.isequal(isequal(2i, 3i, 41), false);
asserts.isequal(isequal([11, 31, 21], [11, 21, 31], 81), false);
asserts.isequal(isequal([21], 21), true);
asserts.isequal(isequal('test', 31), false);
asserts.isequal(isequal(['ab1'; 'cd1'], 'abcd1'), false);
asserts.isequal(isequal(['ab1'; 'cd1'], 'acbd1'), false);
asserts.isequal(isequal(['ab1'; 'cd1'], ['a1'; 'b1'; 'c1'; 'd1']), false);
asserts.isequal(isequal(['ab1'; 'cd1'], ['a1'; 'c1'; 'b1'; 'd1']), false);
asserts.isequal(isequal(['a1b1'; 'c1d1'], ['a1', 'b1'; 'c1', 'd1']), true);
asserts.isequal(isequal(['ab1'; 'cd1'], [97, 98, double('1'); 99, 100, double('1')]), true);
asserts.isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i - 8i, {{9 + 2i}, [2, 3, 4i]}}, {'test_nelson', ['ab'; 'cd'], 2i - 8i, {{9 + 2i}, [2, 3, 0]}}), false);
asserts.isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i - 8i, {{9 + 2i}, [2, 3, 4i]}}, {'test_nelson', ['ab'; 'cd'], 2i - 8i, {{7 + 2i}, [2, 3, 4i]}}), false);
asserts.isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i - 8i, {{9 + 2i}, [2, 3, 4i]}}, {'test_nelson', ['ab'; 'cd'], 2i - 8i, {{8 + 2i}, [2, 3, 4i]}}), false);
asserts.isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i - 8i}, {'test_nelson2', ['ab'; 'cd'], 21i - 81i}), false);
asserts.isequal(isequal({'test_nelson', ['ab'; 'cd'], 21i - 81i}, {'test_nelson', ['ab'; 'cd'], 21i - 81i}), true);
asserts.isequal(isequal({11, 21, 31}, {31; 41; 51}), false);
asserts.isequal(isequal({11, 21, 31}, {11, 21, 51}), false);
asserts.isequal(isequal({11, 21, 31}, {11, 21, 5i - 31}), false);
%=============================================================================
asserts.isequal(isequal(false, false), true);
asserts.isequal(isequal(false, true), false);
%=============================================================================
asserts.isequal(isequal(single(1), double(1)), true);
asserts.isequal(isequal(single(1), single(2)), false);
asserts.isequal(isequal(single(1), single(1)), true);
asserts.isequal(isequal(single(ones(3, 3, 3)), single(ones(3, 3, 3))), true);
%=============================================================================
asserts.isequal(isequal(int8(1), double(1)), true);
asserts.isequal(isequal(int8(1), int8(1)), true);
asserts.isequal(isequal(int8(ones(3, 3, 3)), int8(ones(3, 3, 3))), true);
asserts.isequal(isequal(int16(ones(3, 3, 3)), int16(ones(3, 3, 3))), true);
asserts.isequal(isequal(int32(ones(3, 3, 3)), int32(ones(3, 3, 3))), true);
asserts.isequal(isequal(int64(ones(3, 3, 3)), int64(ones(3, 3, 3))), true);
%=============================================================================
asserts.isequal(isequal(int8(ones(3, 3, 3)), int16(ones(3, 3, 3))), true);
asserts.isequal(isequal(int16(ones(3, 3, 3)), int32(ones(3, 3, 3))), true);
asserts.isequal(isequal(int32(ones(3, 3, 3)), int64(ones(3, 3, 3))), true);
asserts.isequal(isequal(int64(ones(3, 3, 3)), int8(ones(3, 3, 3))), true);
%=============================================================================
A = string(NaN);
B = string(NaN);
asserts.isequal(isequal(A, B), false);
%=============================================================================
A = string(NaN);
B = "";
asserts.isequal(isequal(A, B), false);
%=============================================================================
