%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
msgType = _('Invalid data type. First and second arguments must be integer or double.');
msgFirst = _('First input must be an integer or double array.');
msgMixed = _('Integers can only be combined with integers of the same class, or scalar doubles.');
msgMixedValue = _('A double combined with an integer must be a whole number that fits the integer class.');
msgDouble = _('Double inputs must be nonnegative whole numbers less than 2^64.');
msgAssumedRange = _('Double inputs must have integer values in the range of ASSUMEDTYPE.');
msgAssumedInt = _('Integer inputs cannot be assumed as any other types than their own types.');
msgShift = _('bitshift: the shift amount must be a real integer.');
msgBit = _('Bit position out of range.');
msgSize = _('Inputs must have the same size.');
%=============================================================================
% single values are rejected by the bit operators
assert_checkerror('bitand(single(5), single(3))', msgType);
assert_checkerror('bitor(single(4), 1)', msgType);
assert_checkerror('bitxor(4, single(1))', msgType);
assert_checkerror('bitand(single([]), 3)', msgType);
assert_checkerror('bitshift(single(3), 1)', msgType);
assert_checkerror('bitshift(3, single(1))', msgType);
assert_checkerror('bitshift(single([]), 1)', msgType);
assert_checkerror('bitget(single(5), 1)', msgFirst);
assert_checkerror('bitget(5, single(3))', msgType);
assert_checkerror('bitset(single(5), 2)', msgType);
assert_checkerror('bitset(5, single(2))', msgType);
assert_checkerror('bitcmp(single(5))', msgFirst);
assert_checkerror('bitcmp(true)', msgFirst);
assert_checkerror('bitand(int8(12), single(10))', msgType);
%=============================================================================
% a single shift or bit position is accepted with an integer value
assert_isequal(bitshift(int8(100), single(1)), int8(-56));
assert_isequal(bitshift(uint64(100), single(1)), uint64(200));
assert_isequal(bitget(uint16(100), single(3)), uint16(1));
assert_isequal(bitset(int32(100), single(1)), int32(101));
%=============================================================================
% logical values
assert_isequal(bitand(true, true), true);
assert_isequal(bitxor(true, 3), 2);
assert_isequal(bitor(3, true), 3);
R = bitand(true, true, 'uint8');
assert_isequal(R, 1);
assert_isequal(class(R), 'double');
assert_checkerror('bitand(int8(5), true)', msgMixed);
%=============================================================================
% integers mix only with scalar doubles holding values of their class
assert_isequal(bitand(int8(12), 10), int8(8));
assert_isequal(bitor(10, uint32(12)), uint32(14));
assert_isequal(bitxor(int64(12), 10), int64(6));
assert_checkerror('bitand(int8(12), [10 3])', msgMixed);
assert_checkerror('bitand(int8(5), int16(3))', msgMixed);
assert_checkerror('bitand(int8(5), 300)', msgMixedValue);
assert_checkerror('bitor(uint8(5), -3)', msgMixedValue);
assert_checkerror('bitxor(int8(5), 3.5)', msgMixedValue);
assert_isequal(bitand(int8(-5), int8(3)), int8(3));
%=============================================================================
% double values: whole, nonnegative and below 2^64
assert_isequal(bitand(2^53+2, 2^53+3), 9007199254740992);
assert_isequal(bitand(2^63, 2^63), 2^63);
assert_isequal(bitor(2^60, 2^60+2^20), 2^60+2^20);
assert_isequal(bitand(-0, 3), 0);
assert_checkerror('bitand(2^64, 1)', msgDouble);
assert_checkerror('bitor(Inf, 3)', msgDouble);
assert_checkerror('bitxor(NaN, 3)', msgDouble);
assert_checkerror('bitand(-5, 3)', msgDouble);
assert_checkerror('bitand(5.5, 3)', msgDouble);
%=============================================================================
% assumed type with double values (negative values allowed for signed types)
R = bitand(-5, 3, 'int8');
assert_isequal(R, 3);
assert_isequal(class(R), 'double');
assert_isequal(bitor(-5, 3, 'int16'), -5);
assert_isequal(bitxor(-5, -3, 'int8'), 6);
assert_isequal(bitand(5, 3, "int8"), 1);
assert_checkerror('bitand(200, 3, ''int8'')', msgAssumedRange);
assert_checkerror('bitor(5.5, 3, ''int8'')', msgAssumedRange);
assert_checkerror('bitxor(int8(5), int8(3), ''uint8'')', msgAssumedInt);
assert_isequal(bitand(int8(5), int8(3), 'int8'), int8(1));
%=============================================================================
% implicit expansion and empty inputs
assert_isequal(bitand([1 2 3], [1; 2]), [1 0 1; 0 2 2]);
assert_isequal(bitor(uint8([1 2 3]), uint8([1; 2])), uint8([1 3 3; 3 2 3]));
assert_isequal(bitand([], 3), []);
assert_isequal(size(bitxor(zeros(0, 3), 1)), [0 3]);
assert_isequal(bitand(uint8([]), uint8(1)), uint8([]));
assert_checkerror('bitand([1 2], [1 2 3])', msgSize);
%=============================================================================
% bitshift
assert_isequal(bitshift(1, 63), 2^63);
assert_isequal(bitshift(1, 64), 0);
assert_isequal(bitshift(3, 52), 13510798882111488);
assert_isequal(bitshift(2^53+2, 1), 18014398509481988);
assert_isequal(bitshift(12, -10), 0);
assert_isequal(bitshift(3, int8(1)), 6);
assert_isequal(bitshift(-100, 1, 'int8'), 56);
assert_isequal(bitshift(100, 30, 'int64'), 107374182400);
assert_isequal(bitshift([], 1), []);
assert_isequal(bitshift(3, []), []);
assert_checkerror('bitshift(2^64, -1)', msgDouble);
assert_checkerror('bitshift(-3, 1)', msgDouble);
assert_checkerror('bitshift(Inf, 1)', msgDouble);
assert_checkerror('bitshift(3, 1.5)', msgShift);
assert_checkerror('bitshift(3, Inf)', msgShift);
assert_checkerror('bitshift(int8(3), -Inf)', msgShift);
assert_checkerror('bitshift(3, NaN)', msgShift);
assert_checkerror('bitshift(int8(5), 1, ''uint8'')', msgAssumedInt);
%=============================================================================
% bitcmp: double values use uint64 unless a type is given
R = bitcmp(5);
assert_isequal(R, 18446744073709551610);
assert_isequal(class(R), 'double');
assert_isequal(bitcmp(2^53), 18437736874454810623);
assert_isequal(bitcmp([]), []);
assert_isequal(bitcmp(5, 'uint32'), 4294967290);
assert_isequal(bitcmp(-5, 'int8'), 4);
assert_isequal(bitcmp(int16([0 5 100])), int16([-1 -6 -101]));
assert_isequal(bitcmp(uint64(5), "uint64"), intmax('uint64') - 5);
assert_checkerror('bitcmp(-5)', msgDouble);
assert_checkerror('bitcmp(300, ''uint8'')', msgAssumedRange);
assert_checkerror('bitcmp(uint8(5), ''int8'')', msgAssumedInt);
%=============================================================================
% bitget
assert_isequal(bitget(5, 64), 0);
assert_isequal(bitget(2^60, 61), 1);
assert_isequal(bitget(-5, 1, 'int8'), 1);
assert_isequal(bitget(int8(5), 1, 'int8'), int8(1));
assert_isequal(bitget(5, []), []);
assert_checkerror('bitget(5, 65)', msgBit);
assert_checkerror('bitget(-5, 1)', msgDouble);
assert_checkerror('bitget(int8(5), 1, ''uint8'')', msgAssumedInt);
%=============================================================================
% bitset
assert_isequal(bitset(5, 64), 2^63 + 5);
assert_isequal(bitset(5, 2, 7), 7);
assert_isequal(bitset(5, 2, -1), 7);
assert_isequal(bitset(5, 8, 'uint8'), 133);
assert_isequal(bitset(5, 8, 1, 'uint8'), 133);
assert_isequal(bitset(-5, 8, 'int8'), -5);
assert_isequal(bitset(-5, 1, 0, 'int8'), -6);
assert_isequal(bitset(int8(5), 8, 'int8'), int8(-123));
assert_isequal(bitset(uint8([1 2 3]), 2, [1 0 1]), uint8([3 0 3]));
assert_isequal(bitset([], 1), []);
assert_isequal(bitset(5, []), []);
assert_checkerror('bitset(5, 0)', msgBit);
assert_checkerror('bitset(5, 1.5)', msgBit);
assert_checkerror('bitset(5, 9, ''uint8'')', msgBit);
assert_checkerror('bitset(uint16(0), 65)', msgBit);
assert_checkerror('bitset(-5, 1)', msgDouble);
assert_checkerror('bitset(int8(5), 8, ''uint8'')', msgAssumedInt);
assert_checkerror('bitset([1 2 3], [1; 2])', msgSize);
assert_checkerror('bitset([1 2 3], 2, [1; 0])', msgSize);
%=============================================================================
