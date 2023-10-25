%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(int8(255), int8(127));
assert_isequal(int16(33333), int16(32767));
assert_isequal(int32(2147483647 * 2), int32(2147483647));
assert_isequal(int64(9223372036854775807 + 10), int64(9223372036854775807));
assert_isequal(uint8(double(intmax('uint8')) * 2), intmax('uint8'));
assert_isequal(uint16(double(intmax('uint16')) * 2), intmax('uint16'));
assert_isequal(uint32(double(intmax('uint32')) * 2), intmax('uint32'));
assert_isequal(uint64(double(intmax('uint64')) * 2), intmax('uint64'));
%=============================================================================
assert_isequal(int8(uint32(double(intmax('uint32')) * 2)), intmax('int8'));
%=============================================================================
assert_isequal(int8(double(intmin('int8')) - 1), intmin('int8'))
assert_isequal(int16(double(intmin('int16')) - 1), intmin('int16'))
assert_isequal(int32(double(intmin('int32')) - 1), intmin('int32'))
assert_isequal(int64(double(intmin('int64')) - 1), intmin('int64'))
assert_isequal(uint8(double(intmin('uint8')) - 1), intmin('uint8'))
assert_isequal(uint16(double(intmin('uint16')) - 1), intmin('uint16'))
assert_isequal(uint32(double(intmin('uint32')) - 1), intmin('uint32'))
assert_isequal(uint64(double(intmin('uint64')) - 1), intmin('uint64'))
%=============================================================================
assert_isequal(int64(int16([3 4])), int64([3 4]));
%=============================================================================
assert_isequal(int8(uint8([128 -1])), int8([127    0]));
%=============================================================================
assert_isequal(uint8([-Inf, -1, 300, Inf, NaN]), uint8([0     0   255   255     0]));
%=============================================================================
A = int8(1);
B = double(1);
R = (A + B);
REF = int8(2);
assert_isequal(R, REF);
%=============================================================================
A = int16(3);
R = uint16(A);
REF = uint16(3);
assert_isequal(R, REF);
%=============================================================================
lastwarn('');
%=============================================================================
int64(81997179153022977);
[msg, ID] = lastwarn();
assert_isequal(ID , 'Nelson:int64:InputExceedsFlintmax');
%=============================================================================
lastwarn('');
int64(-72057594035891654);
[msg, ID] = lastwarn();
assert_isequal(ID , 'Nelson:int64:InputExceedsFlintmax');
%=============================================================================
lastwarn('');
uint64(81997179153022977);
[msg, ID] = lastwarn();
lastwarn('');
assert_isequal(ID , 'Nelson:uint64:InputExceedsFlintmax');
%=============================================================================
