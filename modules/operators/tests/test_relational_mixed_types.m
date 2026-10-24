%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% integer compared with logical or char: the integer class is kept
%=============================================================================
asserts.isequal(int8(5) == true, false);
asserts.isequal(int8(1) == true, true);
asserts.isequal(int8(5) ~= true, true);
asserts.isequal(int8(5) > true, true);
asserts.isequal(true < int8(5), true);
asserts.isequal(int8([1 2 3]) == true, [true false false]);
asserts.isequal(int8([1 2 3]) == [true true true], [true false false]);
asserts.isequal(int8(97) == 'a', true);
asserts.isequal(int8(-1) < uint8(255), true);
%=============================================================================
% char compared with logical: code points as doubles
asserts.isequal('abc' == true, [false false false]);
%=============================================================================
% 64-bit integers compared with doubles exactly (no conversion to double)
x = int64(9007199254740992) + int64(1);
asserts.isequal(x == 9007199254740992, false);
asserts.isequal(x > 9007199254740992, true);
asserts.isequal(x ~= 9007199254740992, true);
asserts.isequal(9007199254740992 < x, true);
asserts.isequal(int64(5) < 5.5, true);
asserts.isequal(int64(5) == 5.5, false);
asserts.isequal(int64(5) > 4.5, true);
asserts.isequal(int64(5) == NaN, false);
asserts.isequal(int64(5) ~= NaN, true);
asserts.isequal(int64(5) < NaN, false);
asserts.isequal(int64(9223372036854775807) < 9.3e18, true);
asserts.isequal(int64(-9223372036854775808) > -9.3e18, true);
asserts.isequal(uint64(18446744073709551615) == 1.8446744073709552e19, false);
asserts.isequal(uint64(18446744073709551615) < 1.8446744073709552e19, true);
asserts.isequal(uint64(0) > -0.5, true);
%=============================================================================
% empty operands still need compatible sizes
assert_checkerror('zeros(0,3) == zeros(0,2)', _('Size mismatch on arguments to arithmetic operator =='));
assert_checkerror('[] == zeros(0,3)', _('Size mismatch on arguments to arithmetic operator =='));
R = zeros(0, 3) == zeros(0, 3);
asserts.isequal(size(R), [0 3]);
asserts.isequal(class(R), 'logical');
%=============================================================================
