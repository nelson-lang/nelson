%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% mod/rem(intmin, -1) must not crash and returns 0
asserts.isequal(mod(intmin('int32'), int32(-1)), int32(0));
asserts.isequal(mod(intmin('int8'), int8(-1)), int8(0));
asserts.isequal(mod(intmin('int16'), int16(-1)), int16(0));
asserts.isequal(mod(intmin('int64'), int64(-1)), int64(0));
asserts.isequal(rem(intmin('int8'), int8(-1)), int8(0));
asserts.isequal(rem(intmin('int32'), int32(-1)), int32(0));
asserts.isequal(rem(intmin('int64'), int64(-1)), int64(0));
%=============================================================================
% integer mod is floored (sign of the divisor), rem is truncated
asserts.isequal(mod(int8(-3), int8(2)), int8(1));
asserts.isequal(mod(int16(-7), int16(3)), int16(2));
asserts.isequal(mod(int8(3), int8(-2)), int8(-1));
asserts.isequal(mod(int8(5), int8(2)), int8(1));
asserts.isequal(mod(int8(-5), int8(-2)), int8(-1));
asserts.isequal(rem(int8(-3), int8(2)), int8(-1));
asserts.isequal(rem(int16(-7), int16(3)), int16(-1));
asserts.isequal(rem(int8(3), int8(-2)), int8(1));
% mod with zero divisor keeps the dividend
asserts.isequal(mod(int8(-3), int8(0)), int8(-3));
asserts.isequal(rem(int8(-3), int8(0)), int8(0));
% unsigned
asserts.isequal(mod(uint8(7), uint8(3)), uint8(1));
asserts.isequal(mod(uint8(5), uint8(255)), uint8(5));
%=============================================================================
% mod(integer, non-integer double): computed in double then saturated
asserts.isequal(mod(int8(5), 2.5), int8(0));
asserts.isequal(mod(int8(5), int8(2)), int8(1));
asserts.isequal(mod(-3, int8(2)), int8(1));
asserts.isequal(mod(int8(-3), 2), int8(1));
%=============================================================================
% mod/rem with infinite divisor (floating point)
asserts.isequal(mod(5, Inf), 5);
asserts.isequal(mod(-5, Inf), Inf);
asserts.isequal(mod(5, -Inf), -Inf);
asserts.isequal(mod(-5, -Inf), -5);
asserts.isequal(mod(0, Inf), 0);
asserts.isequal(rem(5, Inf), 5);
asserts.isequal(rem(-5, Inf), -5);
asserts.isequal(rem(5, -Inf), 5);
asserts.isequal(rem(-5, -Inf), -5);
assert(isnan(mod(Inf, Inf)));
assert(isnan(rem(Inf, Inf)));
%=============================================================================
% single precision
R = mod(single(-5), single(Inf));
asserts.isequal(R, single(Inf));
asserts.isequal(class(R), 'single');
R = rem(single(5), single(Inf));
asserts.isequal(R, single(5));
asserts.isequal(class(R), 'single');
R = mod(single(5), single(Inf));
asserts.isequal(R, single(5));
%=============================================================================
