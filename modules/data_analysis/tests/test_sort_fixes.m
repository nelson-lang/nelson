%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(sort(int8([-128 5 -5 127 0]),'ComparisonMethod','abs'), [int8(0) int8(5) int8(-5) int8(127) int8(-128)]);
asserts.isequal(sort(int8([-128 127 -127 127]),'descend','ComparisonMethod','abs'), [int8(-128) int8(-127) int8(127) int8(127)]);
asserts.isequal(sort(int16([-32768 5 -5 32767 0]),'ComparisonMethod','abs'), [int16(0) int16(5) int16(-5) int16(32767) int16(-32768)]);
asserts.isequal(sort(int32([-2147483648 5 -5 2147483647 0]),'ComparisonMethod','abs'), [int32(0) int32(5) int32(-5) int32(2147483647) int32(-2147483648)]);
asserts.isequal(sort(int32([-2147483648 2147483647 -2147483647]),'ComparisonMethod','abs'), [int32(2147483647) int32(-2147483647) int32(-2147483648)]);
asserts.isequal(sort(int32([-2147483648 2147483647 -2147483647]),'descend','ComparisonMethod','abs'), [int32(-2147483648) int32(-2147483647) int32(2147483647)]);
asserts.isequal(sort(int8([3 -1 2;1 -5 4]),2,'ComparisonMethod','abs'), reshape([int8(-1) int8(1) int8(2) int8(4) int8(3) int8(-5)], [2 3]));
asserts.isequal(sort(int8([3 -1 2;1 -5 4]),1,'ComparisonMethod','abs'), reshape([int8(1) int8(3) int8(-1) int8(-5) int8(2) int8(4)], [2 3]));
asserts.isequal(sort(int8([3 -1 -3 1 0 -0]),'ComparisonMethod','abs'), [int8(0) int8(0) int8(1) int8(-1) int8(3) int8(-3)]);
asserts.isequal(sort(int8(cat(3,[1 -2],[-3 0])),3,'ComparisonMethod','abs'), reshape([int8(1) int8(0) int8(-3) int8(-2)], [1 2 2]));
asserts.isequal(sort(int8([]),'ComparisonMethod','abs'), zeros([0 0], 'int8'));
asserts.isequal(sort(int8(zeros(0,3)),'ComparisonMethod','abs'), zeros([0 3], 'int8'));
asserts.isequal(sort(sparse([3 0 1])), sparse([0 1 3]));
asserts.isequal(sort(sparse([3 0 1]),'descend'), sparse([3 1 0]));
asserts.isequal(sort(sparse([3 0;1 2]),2), sparse(reshape([0 1 3 2], [2 2])));
asserts.isequal(sort(sparse([3 0;1 2])), sparse(reshape([1 3 0 2], [2 2])));
asserts.isequal(sort(sparse([3 0;1 2]),1,'descend'), sparse(reshape([3 1 2 0], [2 2])));
asserts.isequal(sort(sparse(3,0)), sparse(zeros([3 0], 'double')));
asserts.isequal(sort(sparse([])), sparse(zeros([0 0], 'double')));
asserts.isequal(sort(sparse([1 NaN 0])), sparse([0 1 NaN]));
asserts.isequal(sort(sparse([1+1i 0 -2])), sparse(complex([0 1 -2], [0 1 0])));
asserts.isequal(sort(sparse([true false true])), sparse(logical([0 1 1])));
asserts.isequal(sort(int8(-5:5),'ComparisonMethod','abs'), [int8(0) int8(1) int8(-1) int8(2) int8(-2) int8(3) int8(-3) int8(4) int8(-4) int8(5) int8(-5)]);
asserts.isequal(sort([intmin('int8') intmax('int8')],'ComparisonMethod','abs'), [int8(127) int8(-128)]);
asserts.isequal(sort([intmin('int8') intmax('int8')],'descend','ComparisonMethod','abs'), [int8(-128) int8(127)]);
asserts.isequal(sort([intmax('int8') intmin('int8')],'ComparisonMethod','abs'), [int8(127) int8(-128)]);
asserts.isequal(sort([intmax('int8') intmin('int8')],'descend','ComparisonMethod','abs'), [int8(-128) int8(127)]);
asserts.isequal(sort([intmax('int64') intmin('int64')],'ComparisonMethod','abs'), [(int64(9223372036)*int64(1000000000) + int64(854775807)) (-int64(9223372036)*int64(1000000000) - int64(854775808))]);
asserts.isequal(sort([intmin('int64') intmax('int64') intmin('int64')+1],'ComparisonMethod','abs'), [(int64(9223372036)*int64(1000000000) + int64(854775807)) (-int64(9223372036)*int64(1000000000) - int64(854775807)) (-int64(9223372036)*int64(1000000000) - int64(854775808))]);
asserts.isequal(sort([intmin('int64') intmax('int64') intmin('int64')+1],'descend','ComparisonMethod','abs'), [(-int64(9223372036)*int64(1000000000) - int64(854775808)) (-int64(9223372036)*int64(1000000000) - int64(854775807)) (int64(9223372036)*int64(1000000000) + int64(854775807))]);
asserts.isequal(sort(int8([])), zeros([0 0], 'int8'));
asserts.isequal(sort(zeros(0,3,'int8'),'ComparisonMethod','abs'), zeros([0 3], 'int8'));
%=============================================================================
