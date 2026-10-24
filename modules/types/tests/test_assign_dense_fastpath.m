%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% Dense same-class indexed assignment `dest(index) = rhs` goes through the
% memcpy fast path. These tests pin the two things that path can get wrong:
% the index -> destination-slot mapping (non-contiguous, permuted indices)
% and the per-class element byte width (complex, char, logical, all ints).
%=============================================================================
% double, permuted non-contiguous indices (advance == 1)
c = [1 2 3 4 5];
c([4 1 3]) = [40 10 30];
assert_isequal(c, [10 2 30 40 5]);
%=============================================================================
% double, scalar broadcast (advance == 0)
c = [1 2 3 4];
c(:) = 7;
assert_isequal(c, [7 7 7 7]);
assert_isequal(class(c), 'double');
%=============================================================================
% single, same-class, reversed order
c = single([1 2 3]);
c([3 1]) = single([30 10]);
assert_isequal(c, single([10 2 30]));
assert_isequal(class(c), 'single');
%=============================================================================
% double complex: both real and imaginary parts must follow the mapping
c = [1+2i, 3+4i, 5+6i];
c([3 1]) = [30+31i, 10+11i];
assert_isequal(c, [10+11i, 3+4i, 30+31i]);
assert_isequal(class(c), 'double');
assert_istrue(~isreal(c));
%=============================================================================
% single complex, scalar broadcast
c = single([1+1i, 2+2i, 3+3i]);
c(:) = single(9+8i);
assert_isequal(c, single([9+8i, 9+8i, 9+8i]));
assert_isequal(class(c), 'single');
%=============================================================================
% char, permuted indices
c = 'abcd';
c([4 1]) = 'ZY';
assert_isequal(c, 'YbcZ');
assert_isequal(class(c), 'char');
%=============================================================================
% logical
c = [true false true false];
c([1 4]) = [false true];
assert_isequal(c, [false false true true]);
assert_isequal(class(c), 'logical');
%=============================================================================
% every integer class, same-class RHS
c = int8([1 2 3 4]);   c([2 4]) = int8([-5 6]);   assert_isequal(c, int8([1 -5 3 6]));
c = uint8([1 2 3]);    c(:) = uint8(200);         assert_isequal(c, uint8([200 200 200]));
c = int16([1 2 3]);    c([3 1]) = int16([30 10]); assert_isequal(c, int16([10 2 30]));
c = uint16([1 2 3]);   c(2) = uint16(65000);      assert_isequal(c, uint16([1 65000 3]));
c = int32([1 2 3]);    c([1 3]) = int32([10 30]); assert_isequal(c, int32([10 2 30]));
c = uint32([1 2 3]);   c(2) = uint32(70000);      assert_isequal(c, uint32([1 70000 3]));
c = int64([1 2 3 4]);  c([4 2]) = int64([40 20]); assert_isequal(c, int64([1 20 3 40]));
c = uint64([1 2 3]);   c(:) = uint64(9);          assert_isequal(c, uint64([9 9 9]));
%=============================================================================
% 2D destination, linear indexing keeps column-major slot mapping
c = reshape(int32(1:6), 2, 3);
c([1 6 3]) = int32([100 600 300]);
expected = reshape(int32([100 2 300 4 5 600]), 2, 3);
assert_isequal(c, expected);
%=============================================================================
% larger permutation to exercise the copy loop
n = 64;
c = double(1:n);
perm = n:-1:1;
c(perm) = double((1:n) * 10);
assert_isequal(c, double((n:-1:1) * 10));
%=============================================================================
