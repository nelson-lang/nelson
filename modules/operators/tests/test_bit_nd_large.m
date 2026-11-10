%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
msgSize = _('Inputs must have the same size.');
msgShiftSize = _('bitshift: arrays must be the same size or one must be scalar.');
A3 = reshape(0:23, 2, 3, 4);
U3 = uint16(A3);
%=============================================================================
% N-D arrays and implicit expansion over N-D shapes
assert_isequal(bitand(A3, 5), reshape([0 1 0 1 4 5 4 5 0 1 0 1 4 5 4 5 0 1 0 1 4 5 4 5], 2, 3, 4));
assert_isequal(bitor(A3, reshape(1:4, 1, 1, 4)), ...
  reshape([1 1 3 3 5 5 6 7 10 11 10 11 15 15 15 15 19 19 22 23 20 21 22 23], 2, 3, 4));
assert_isequal(bitxor(A3, [1; 2]), reshape([1 3 3 1 5 7 7 5 9 11 11 9 13 15 15 13 17 19 19 17 21 23 23 21], 2, 3, 4));
assert_isequal(bitand(A3, [1 2 3]), reshape([0 1 2 2 0 1 0 1 0 0 2 3 0 1 2 2 0 1 0 1 0 0 2 3], 2, 3, 4));
assert_isequal(bitand(U3, uint16(reshape(1:4, 1, 1, 4))), ...
  uint16(reshape([0 1 0 1 0 1 2 2 0 0 2 2 0 1 2 3 0 1 0 0 4 4 4 4], 2, 3, 4)));
assert_isequal(bitor(ones(2, 1, 3), [1 2 4 8]), repmat([1 3 5 9; 1 3 5 9], 1, 1, 3));
assert_isequal(bitxor(uint8(ones(2, 1, 3)), uint8([1 2 4 8])), uint8(repmat([0 3 5 9; 0 3 5 9], 1, 1, 3)));
assert_isequal(bitand(ones(2, 3), ones(2, 3, 2)), ones(2, 3, 2));
assert_isequal(bitand(reshape(1:6, 1, 2, 3), reshape(1:4, 4, 1)), ...
  reshape([1 0 1 0 0 2 2 0 1 2 3 0 0 0 0 4 1 0 1 4 0 2 2 4], 4, 2, 3));
assert_checkerror('bitand(reshape(0:23, 2, 3, 4), ones(3, 2))', msgSize);
%=============================================================================
assert_isequal(bitshift(A3, 2), A3 * 4);
assert_isequal(bitshift(A3, reshape(mod(0:23, 4), 2, 3, 4)), ...
  reshape([0 2 8 24 4 10 24 56 8 18 40 88 12 26 56 120 16 34 72 152 20 42 88 184], 2, 3, 4));
assert_isequal(bitshift(U3, -1), uint16(floor(A3 / 2)));
assert_checkerror('bitshift(reshape(0:23, 2, 3, 4), [1 2])', msgShiftSize);
assert_isequal(bitcmp(U3), 65535 - U3);
assert_isequal(bitcmp(A3, 'uint8'), 255 - A3);
%=============================================================================
assert_isequal(bitget(A3, 3), reshape([0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1], 2, 3, 4));
assert_isequal(bitget(U3, reshape(mod(0:23, 5) + 1, 2, 3, 4)), ...
  uint16(reshape([0 0 0 0 0 1 1 1 1 0 0 1 1 1 0 1 0 0 0 1 0 0 1 0], 2, 3, 4)));
assert_checkerror('bitget(reshape(0:23, 2, 3, 4), [1 2])', msgSize);
%=============================================================================
assert_isequal(bitset(A3, 6), A3 + 32);
assert_isequal(bitset(U3, 1, 0), uint16(2 * floor(A3 / 2)));
assert_isequal(bitset(A3, reshape(mod(0:23, 5) + 1, 2, 3, 4)), ...
  reshape([1 3 6 11 20 5 6 7 8 25 11 11 12 13 30 15 18 21 26 19 21 23 22 31], 2, 3, 4));
assert_isequal(bitset(A3, 2, reshape(mod(0:23, 2), 2, 3, 4)), ...
  reshape([0 3 0 3 4 7 4 7 8 11 8 11 12 15 12 15 16 19 16 19 20 23 20 23], 2, 3, 4));
assert_checkerror('bitset(reshape(0:23, 2, 3, 4), 2, [1 0])', msgSize);
assert_isequal(bitset(5, 2, reshape([1 0 1 0], 2, 1, 2)), reshape([7 5 7 5], 2, 1, 2));
assert_isequal(bitset(5, reshape(1:4, 2, 1, 2), 1), reshape([5 7 5 13], 2, 1, 2));
assert_isequal(bitset(uint8(5), [1 2; 3 4], [1 0; 0 1]), uint8([5 5; 1 13]));
assert_isequal(bitset(5, [1 2; 3 4], [1 0; 0 1], 'int8'), [5 5; 1 13]);
assert_isequal(bitset(-5, [1 8], 'int8'), [-5 -5]);
assert_isequal(bitset(5, 2, true), 7);
assert_isequal(bitset(5, 2, int8(0)), 5);
assert_isequal(bitset(int8(-128), 8, 0), int8(0));
assert_isequal(bitset(uint64(0), 64), bitshift(uint64(1), 63));
assert_isequal(bitset(int64(0), 64), intmin('int64'));
assert_isequal(bitset(0, 64, 'int64'), -2^63);
assert_isequal(bitset(uint8(5), uint16(3)), uint8(5));
assert_checkerror('bitset(5, 2, 1+1i)', _('Input argument must be real.'));
assert_checkerror('bitset(5, 2, ''a'')', _('ASSUMEDTYPE must be an integer type name.'));
assert_isequal(swapbytes(reshape(uint16(1:8), 2, 2, 2)), reshape(uint16(256 * (1:8)), 2, 2, 2));
%=============================================================================
% large arrays (parallel loops)
X = mod((1:3e6) * 7919, 2^20);
assert_isequal(sum(bitand(X, 12345)), 18517524576);
assert_isequal(sum(bitxor(X, flip(X))), 1148537683264);
assert_isequal(sum(bitor(X, 7, 'uint32')), 1572855117376);
assert_isequal(sum(bitshift(X, 3)), 12582756939008);
assert_isequal(sum(bitshift(X, -2, 'int32')), 393210029344);
assert_isequal(sum(mod(bitcmp(X, 'uint32'), 9973)), 14955530201);
assert_isequal(sum(bitget(X, 5)), 1500000);
assert_isequal(sum(double(bitget(uint32(X), 7))), 1500000);
assert_isequal(sum(bitset(X, 21)), 4718572617376);
assert_isequal(sum(bitset(X, 3, 0)), 1572838617376);
assert_isequal(sum(double(bitset(uint32(X), 9, mod(1:3e6, 2)))), 1572844618912);
assert_isequal(sum(bitand(mod((1:2000) * 7919, 2^20), mod((1:1500)' * 7919, 2^20)), 'all'), 766842118928);
R = bitxor(uint32(mod((1:2000)' * 7919, 2^20)), uint32(mod((1:1500) * 7919, 2^20)));
assert_isequal(sum(double(R), 'all'), 1572646378144);
assert_isequal(sum(double(swapbytes(uint32(X)))), 6442358385381376);
%=============================================================================
