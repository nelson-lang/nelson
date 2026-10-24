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
% Hexadecimal (0x/0X) and binary (0b/0B) integer literals.
%=============================================================================
% Basic hexadecimal values and default (unsigned, digit-count) typing.
asserts.isequal(0xFF, uint8(255));
asserts.isequal(class(0xFF), 'uint8');
asserts.isequal(0xff, uint8(255));
asserts.isequal(0X1A, uint8(26));
asserts.isequal(0x0, uint8(0));
asserts.isequal(0xABCDEF, uint32(11259375));
asserts.isequal(0xFFE2, uint16(65506));
%=============================================================================
% Auto-sizing is by digit count (four bits per hex digit), not by value.
asserts.isequal(class(0xFF), 'uint8');
asserts.isequal(class(0x100), 'uint16');
asserts.isequal(class(0xFFFF), 'uint16');
asserts.isequal(class(0x10000), 'uint32');
asserts.isequal(class(0xFFFFFFFF), 'uint32');
asserts.isequal(class(0x100000000), 'uint64');
asserts.isequal(0x100000000, uint64(4294967296));
asserts.isequal(0xFFFFFFFFFFFFFFFF, uint64(18446744073709551615));
% Leading zeros count towards the width.
asserts.isequal(class(0x0FF), 'uint16');
asserts.isequal(0x0FF, uint16(255));
asserts.isequal(class(0x000000FF), 'uint32');
asserts.isequal(class(0x00000000000000FF), 'uint64');
%=============================================================================
% Basic binary values and default typing (one bit per digit).
asserts.isequal(0b1010, uint8(10));
asserts.isequal(class(0b1010), 'uint8');
asserts.isequal(0B1111, uint8(15));
asserts.isequal(0b0, uint8(0));
asserts.isequal(class(0b11111111), 'uint8');
asserts.isequal(class(0b111111111), 'uint16');
asserts.isequal(0b111111111, uint16(511));
asserts.isequal(0b100000000, uint16(256));
%=============================================================================
% Unsigned type suffixes select the exact type.
asserts.isequal(0xFFu8, uint8(255));
asserts.isequal(0xFFu16, uint16(255));
asserts.isequal(0xFFu32, uint32(255));
asserts.isequal(0xFFu64, uint64(255));
asserts.isequal(0b1111u8, uint8(15));
asserts.isequal(class(0xFFu16), 'uint16');
% Uppercase suffix letters are accepted.
asserts.isequal(0xFFU8, uint8(255));
asserts.isequal(0xFFU16, uint16(255));
%=============================================================================
% Signed type suffixes reinterpret the bit pattern as two's complement.
asserts.isequal(0xFFs8, int8(-1));
asserts.isequal(class(0xFFs8), 'int8');
asserts.isequal(0x7Fs8, int8(127));
asserts.isequal(0x80s8, int8(-128));
asserts.isequal(0xFFs16, int16(255));
asserts.isequal(0x8000s16, int16(-32768));
asserts.isequal(0xFFFFs16, int16(-1));
asserts.isequal(0xFFs32, int32(255));
asserts.isequal(0xFFs64, int64(255));
asserts.isequal(0b1111s8, int8(15));
asserts.isequal(0b10000000s8, int8(-128));
asserts.isequal(0xFFFFFFFFFFFFFFFFs64, int64(-1));
asserts.isequal(0x8000000000000000s64, int64(-9223372036854775808));
% Uppercase signed suffix.
asserts.isequal(0xFFS8, int8(-1));
%=============================================================================
% Literals compose everywhere a number can appear.
asserts.isequal([0xFF 0x01 0x02], uint8([255 1 2]));
asserts.isequal([0xFF, 0x01; 0x02, 0x03], uint8([255 1; 2 3]));
asserts.isequal(double(0xFF), 255);
asserts.isequal(0x0F + 0x01, uint8(16));
asserts.isequal(0xFFu64 + 1, uint64(256));
% uint8 arithmetic saturates.
asserts.isequal(0xFF + 0x01, uint8(255));
% A trailing quote is transpose, not a character array.
asserts.isequal(0xFF', uint8(255));
%=============================================================================
% Invalid digits.
msg = sprintf(_("Lexical error '%s'"), _('Invalid digit in hexadecimal literal. Supported hexadecimal digits are 0-9 and A-F. Supported type suffixes are u8, u16, u32, u64, and s8, s16, s32, s64.'));
asserts.checkerror('x = 0xGG;', msg);
asserts.checkerror('x = 0x;', msg);
asserts.checkerror('x = 0xFFu;', msg);
asserts.checkerror('x = 0xFFu88;', msg);
asserts.checkerror('x = 0x1.5;', msg);
msgb = sprintf(_("Lexical error '%s'"), _('Invalid digit in binary literal. Supported binary digits are 0 and 1. Supported type suffixes are u8, u16, u32, u64, and s8, s16, s32, s64.'));
asserts.checkerror('x = 0b2;', msgb);
asserts.checkerror('x = 0b101g;', msgb);
%=============================================================================
% Too many digits for the specified or implied type.
msg2 = sprintf(_("Lexical error '%s'"), _('Hexadecimal literal has too many digits for specified or implied type suffix.'));
asserts.checkerror('x = 0xFFFu8;', msg2);
asserts.checkerror('x = 0x1FFs8;', msg2);
asserts.checkerror('x = 0x00FFu8;', msg2);
asserts.checkerror('x = 0x000000000000000000FF;', msg2);
msg2b = sprintf(_("Lexical error '%s'"), _('Binary literal has too many digits for specified or implied type suffix.'));
asserts.checkerror('x = 0b111111111u8;', msg2b);
%=============================================================================
