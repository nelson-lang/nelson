%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% unsigned integer multiplication saturated too late; integer division was
% wrong for 0/0, intmax/-1 and values beyond 2^53
%=============================================================================
% unsigned multiplication saturation
asserts.isequal(uint8(20) * uint8(20), uint8(255));
asserts.isequal(uint8(16) * uint8(16), uint8(255));
asserts.isequal(uint8(15) * uint8(17), uint8(255));
asserts.isequal(uint8(12) * uint8(21), uint8(252));
asserts.isequal(uint16(256) * uint16(256), uint16(65535));
asserts.isequal(uint16(255) * uint16(257), uint16(65535));
%=============================================================================
% integer division by zero
asserts.isequal(int8(0) / int8(0), int8(0));
asserts.isequal(int8(5) / int8(0), int8(127));
asserts.isequal(int8(-5) / int8(0), int8(-128));
asserts.isequal(uint8(0) / uint8(0), uint8(0));
asserts.isequal(uint8(5) / uint8(0), uint8(255));
%=============================================================================
% division by -1: only intmin saturates
asserts.isequal(int8(127) / int8(-1), int8(-127));
asserts.isequal(int8(-128) / int8(-1), int8(127));
asserts.isequal(int8(-127) / int8(-1), int8(127));
%=============================================================================
% division rounding: round half away from zero
asserts.isequal(int64(7) / int64(2), int64(4));
asserts.isequal(int64(-7) / int64(2), int64(-4));
asserts.isequal(int64(5) / int64(2), int64(3));
asserts.isequal(int64(-5) / int64(2), int64(-3));
asserts.isequal(int64(6) / int64(4), int64(2));
%=============================================================================
% int64/uint64 division must be exact beyond 2^53
big = int64(9007199254740992) + int64(1); % 2^53 + 1
asserts.isequal(big / int64(1), big);
asserts.isequal(big / int64(-1), -big);
asserts.isequal(intmax('uint64') / uint64(2), uint64(9223372036854775808));
asserts.isequal(intmin('int64') / int64(-1), intmax('int64'));
%=============================================================================
