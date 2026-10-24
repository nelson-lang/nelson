%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Negative values from signed integer classes are rendered using their
% two's-complement bit pattern over the type's native bit width.
%=============================================================================
R = dec2bin(int8(-128), 8);
REF = '10000000';
assert_isequal(R, REF);
%=============================================================================
R = dec2bin(int8(-92), 8);
REF = '10100100';
assert_isequal(R, REF);
%=============================================================================
R = dec2bin(int8(-1));
REF = '11111111';
assert_isequal(R, REF);
%=============================================================================
R = dec2bin(int16(-1));
REF = '1111111111111111';
assert_isequal(R, REF);
%=============================================================================
R = dec2bin(int32(-1));
REF = repmat('1', 1, 32);
assert_isequal(R, REF);
%=============================================================================
R = dec2bin(int64(-1));
REF = repmat('1', 1, 64);
assert_isequal(R, REF);
%=============================================================================
% Non-negative signed inputs keep their natural width (no padding to type width).
R = dec2bin(int8(5));
REF = '101';
assert_isequal(R, REF);
%=============================================================================
% Unsigned inputs are unchanged.
R = dec2bin(uint8(169), 8);
REF = '10101001';
assert_isequal(R, REF);
%=============================================================================
% dec2hex two's-complement.
R = dec2hex(int8(-1));
REF = 'FF';
assert_isequal(R, REF);
%=============================================================================
R = dec2hex(int8(-128));
REF = '80';
assert_isequal(R, REF);
%=============================================================================
R = dec2hex(int8(-92));
REF = 'A4';
assert_isequal(R, REF);
%=============================================================================
R = dec2hex(int16(-1));
REF = 'FFFF';
assert_isequal(R, REF);
%=============================================================================
% dec2base two's-complement.
R = dec2base(int8(-1), 10);
REF = '255';
assert_isequal(R, REF);
%=============================================================================
R = dec2base(int8(-1), 16);
REF = 'FF';
assert_isequal(R, REF);
%=============================================================================
% Mixed positive / negative signed array is padded to a common width.
R = dec2bin([int8(1); int8(-1)]);
REF = ['00000001'; '11111111'];
assert_isequal(R, REF);
%=============================================================================
% Negative double still errors (unchanged behavior).
assert_checkerror('dec2bin(-1)', _('An array of integers values, 0 <= D <= flintmax expected.'));
%=============================================================================
