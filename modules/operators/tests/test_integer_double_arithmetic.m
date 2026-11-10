%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% integer +/- double scalar: rounded half away from zero, saturated, NaN gives 0;
% 64-bit classes are exact, smaller classes are computed in double
%=============================================================================
asserts.isequal(int8(5) + NaN, int8(0));
asserts.isequal(int8(5) - NaN, int8(0));
asserts.isequal(NaN - int8(5), int8(0));
asserts.isequal(NaN + int8(5), int8(0));
asserts.isequal(uint8(5) + NaN, uint8(0));
asserts.isequal(int64(9007199254740992) + NaN, int64(0));
%=============================================================================
asserts.isequal(int8(5) - Inf, int8(-128));
asserts.isequal(Inf - int8(5), int8(127));
asserts.isequal(-Inf + int8(5), int8(-128));
asserts.isequal(int8(-5) + Inf, int8(127));
asserts.isequal(int8(5) + (-Inf), int8(-128));
%=============================================================================
asserts.isequal(int8(5) - 0.5, int8(5));
asserts.isequal(int8(5) - 1.5, int8(4));
asserts.isequal(int8(5) + (-1.5), int8(4));
asserts.isequal(int8(-5) + 0.5, int8(-5));
asserts.isequal(int8(-5) - 0.5, int8(-6));
asserts.isequal(int8(5) + 2.5, int8(8));
asserts.isequal(int8(4) + 0.5, int8(5));
asserts.isequal(int8(-4) - 0.5, int8(-5));
asserts.isequal(int8([1 2]) + 1.5, int8([3 4]));
asserts.isequal(class(int8(5) + 1.5), 'int8');
%=============================================================================
asserts.isequal(int8(5) + 200, int8(127));
asserts.isequal(int8(5) - 200, int8(-128));
asserts.isequal(int8(5) + -200, int8(-128));
asserts.isequal(int8(5) - 128, int8(-123));
asserts.isequal(int8(5) - 133, int8(-128));
asserts.isequal(int8(120) + 6.5, int8(127));
asserts.isequal(int8(120) + 7.5, int8(127));
%=============================================================================
asserts.isequal(uint8(5) + (-10), uint8(0));
asserts.isequal(uint8(5) - 10, uint8(0));
asserts.isequal(uint8(5) - 4.5, uint8(1));
asserts.isequal(uint8(5) - 5.5, uint8(0));
asserts.isequal(uint8(5) - 0.5, uint8(5));
asserts.isequal(uint8(250) + 4.5, uint8(255));
asserts.isequal(uint8(250) + 5.5, uint8(255));
%=============================================================================
asserts.isequal(int32(5) + 2147483647, int32(2147483647));
asserts.isequal(int32(5) - 2147483648, int32(-2147483643));
asserts.isequal(int32(5) - 4294967296, int32(-2147483648));
%=============================================================================
asserts.isequal(int64(5) + 2.5, int64(8));
asserts.isequal(int64(5) - 2.5, int64(3));
asserts.isequal(int64(5) - 3.5, int64(2));
asserts.isequal(int64(-5) + 2.5, int64(-3));
asserts.isequal(int64(-5) - 2.5, int64(-8));
asserts.isequal(int64(4) - 0.5, int64(4));
asserts.isequal(int64(-4) + 0.5, int64(-4));
asserts.isequal(uint64(5) - 4.5, uint64(1));
asserts.isequal(uint64(5) - 10, uint64(0));
asserts.isequal(uint64(0) - 0.5, uint64(0));
%=============================================================================
% classes of 32 bits or less round the double sum: 3 + 0.49999999999999994 is 3.5 in double
h = 0.49999999999999994;
asserts.isequal(int8(3) + h, int8(4));
asserts.isequal(h + int8(3), int8(4));
asserts.isequal(int8(3) - h, int8(3));
asserts.isequal(h - int8(3), int8(-3));
asserts.isequal(int8(3) - (-h), int8(4));
asserts.isequal(-h - int8(3), int8(-4));
asserts.isequal(int8(-3) - h, int8(-4));
asserts.isequal(int8(-3) + h, int8(-3));
asserts.isequal(uint8(0) + h, uint8(0));
asserts.isequal(uint8(3) + h, uint8(4));
asserts.isequal(uint8(0) + 0.5000000000000001, uint8(1));
asserts.isequal(int32(3) + h, int32(4));
asserts.isequal(int32(7) - (-h), int32(8));
asserts.isequal(uint32(7) + h, uint32(8));
asserts.isequal(int8([3 -3 7]) + h, int8([4 -3 8]));
% large doubles and halves near the class limits
asserts.isequal(int32(3) - 2147483647.5, int32(-2147483645));
asserts.isequal(int32(3) + (-2147483648.5), int32(-2147483646));
asserts.isequal(2147483647.5 - intmax('int32'), int32(1));
asserts.isequal(intmin('int32') + 2147483647.5, int32(-1));
asserts.isequal(uint32(3) + 2147483647.5, uint32(2147483651));
asserts.isequal(intmax('uint32') - 2147483647.5, uint32(2147483648));
asserts.isequal(intmax('uint32') + (-2147483648.5), uint32(2147483647));
asserts.isequal(uint32(0) - (-2147483648.5), uint32(2147483649));
asserts.isequal(int8(3) + (2^52 + 0.5), int8(127));
asserts.isequal(int8(3) - 1e300, int8(-128));
asserts.isequal(-0 - int8(3), int8(-3));
%=============================================================================
% 64-bit classes stay exact
asserts.isequal(int64(3) + h, int64(3));
asserts.isequal(uint64(3) + h, uint64(3));
%=============================================================================
% exact integer scalars keep the saturated integer kernel
x = int32(1:10);
asserts.isequal(x + 1, int32(2:11));
asserts.isequal(x - 2, int32(-1:8));
asserts.isequal(3 - x, int32(2:-1:-7));
%=============================================================================
