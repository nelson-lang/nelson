%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% integer and double scalar with .^ and ^
%=============================================================================
i64 = @(hi, lo) int64(hi) * int64(1000000000) + int64(lo);
u64 = @(hi, lo) uint64(hi) * uint64(1000000000) + uint64(lo);
msg = _('Positive integral powers expected.');
negativeMsg = _('Only positive integers expected.');
%=============================================================================
% a double base is not rounded before the power
asserts.isequal(1.5 .^ int64(3), int64(3));
asserts.isequal(2.5 .^ int64(3), int64(16));
asserts.isequal(-2.5 .^ int64(3), int64(-16));
asserts.isequal(0.5 .^ int64(3), int64(0));
asserts.isequal(1.5 .^ int8(3), int8(3));
asserts.isequal(2.5 .^ uint16([0 1 2 3]), uint16([1 3 6 16]));
asserts.isequal(2.5 ^ int64(3), int64(16));
asserts.isequal(mpower(1.5, int32(3)), int32(3));
asserts.isequal(power(2.5, int64(3)), int64(16));
asserts.isequal(class(1.5 .^ int8([1 2])), 'int8');
%=============================================================================
% integer valued base of the class: exact integer power
asserts.isequal(3 .^ int64(34), i64(16677181, 699666569));
asserts.isequal(3 .^ int64(35), i64(50031545, 98999707));
asserts.isequal(3 .^ uint64(34), u64(16677181, 699666569));
asserts.isequal(-3 .^ int64(35), -i64(50031545, 98999707));
asserts.isequal(3 .^ int64(40), intmax('int64'));
% otherwise binary powering in double
asserts.isequal((-3) .^ uint64(34), u64(16677181, 699666568));
asserts.isequal((-3) .^ int64(34), i64(16677181, 699666569));
asserts.isequal(7.3 .^ int64(19), i64(25300805, 167308476));
asserts.isequal(1.7 .^ int64(70), i64(13533947, 780937478));
asserts.isequal(2.5 .^ int64(44), i64(323117426, 778526464));
asserts.isequal(7.3 .^ int64(25), intmax('int64'));
%=============================================================================
% the parity of a large exponent is exact
asserts.isequal((-1) .^ (int64(2)^53 + 1), int64(-1));
asserts.isequal((-1) .^ (int64(2)^62 + 3), int64(-1));
asserts.isequal((-1) .^ (int64(2)^62), int64(1));
asserts.isequal(0.5 .^ (int64(2)^62), int64(0));
%=============================================================================
asserts.isequal(NaN .^ int64([0 1]), int64([0 0]));
asserts.isequal(Inf .^ int64([0 1]), [int64(1), intmax('int64')]);
asserts.isequal((-Inf) .^ int8([0 1 2]), int8([1 -128 127]));
asserts.isequal(0 .^ int64([0 1]), int64([1 0]));
asserts.isequal(size(2.5 .^ zeros(0, 3, 'int64')), [0 3]);
%=============================================================================
% integer base with a double exponent
asserts.isequal(int64(-3) .^ Inf, intmax('int64'));
asserts.isequal(int64([-1 0 1 2]) .^ Inf, [int64([1 0 1]), intmax('int64')]);
asserts.isequal(int8([-3 -1 0 1 2]) .^ Inf, int8([127 1 0 1 127]));
asserts.isequal(uint8([0 1 2]) .^ Inf, uint8([0 1 255]));
asserts.isequal(int64(3) .^ (2^53 + 1), intmax('int64'));
asserts.isequal(int64(-1) .^ (2^53 + 2), int64(1));
asserts.isequal(int64(-1) .^ 1e300, int64(1));
asserts.isequal(int16([-1 2]) .^ 1e300, int16([1 32767]));
asserts.isequal(int64(-1) .^ 65, int64(-1));
asserts.isequal(int64(-1) .^ 66, int64(1));
asserts.isequal(int64(3) .^ 39, i64(4052555153, 18976267));
asserts.isequal(int64(-2) ^ 3, int64(-8));
asserts.isequal(int8(3) ^ 2, int8(9));
%=============================================================================
% integer exponents beyond 32 bits
asserts.isequal(int64(2) .^ (int64(2)^32), intmax('int64'));
asserts.isequal(int64(-1) .^ (int64(2)^32 + 1), int64(-1));
asserts.isequal(uint64(1) .^ (uint64(2)^40), uint64(1));
%=============================================================================
assert_checkerror('uint64(2) .^ -1', negativeMsg);
assert_checkerror('int64(2) .^ -1', negativeMsg);
assert_checkerror('int8(4) .^ 0.5', msg);
assert_checkerror('int64(2) .^ NaN', msg);
assert_checkerror('int64(2) .^ -Inf', negativeMsg);
assert_checkerror('int64(3) ^ 2.5', msg);
assert_checkerror('0.5 .^ int64(-3)', negativeMsg);
assert_checkerror('2 .^ int8([1 -1])', negativeMsg);
%=============================================================================
% ^ with an integer operand needs two scalars
matrixMsg = _('Integer arguments of ^ must both be scalars; use .^ for element-wise power.');
assert_checkerror('int64([1 2; 3 4]) ^ 2', matrixMsg);
assert_checkerror('2 ^ int64([1 2; 3 4])', matrixMsg);
assert_checkerror('int8([1 2; 3 4]) ^ int8(2)', matrixMsg);
assert_checkerror('int8([1 2; 3 4]) ^ 2.5', matrixMsg);
assert_checkerror('[1 2; 3 4] ^ int8(2)', matrixMsg);
assert_checkerror('int8(3) ^ [1 2; 3 4]', matrixMsg);
assert_checkerror('int8(zeros(0, 0)) ^ 2', matrixMsg);
assert_checkerror('mpower(int8([1 2; 3 4]), 2)', matrixMsg);
asserts.isequal(int8(3) ^ int8(2), int8(9));
%=============================================================================
