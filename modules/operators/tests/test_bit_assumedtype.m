%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% assumedtype bounds the arithmetic for double inputs; the result stays double.
%=============================================================================
% bitshift with assumedtype (double input -> double result)
assert_isequal(bitshift(255, -4, 'uint8'), 15);
assert_isequal(bitshift(255, 4, 'uint8'), 240);
assert_isequal(bitshift(255, 4, 'uint16'), 4080);
assert_isequal(class(bitshift(255, 4, 'uint8')), 'double');
assert_isequal(bitshift(1, 10, 'uint8'), 0);
assert_isequal(bitshift(170, [1 -1], 'uint8'), [84 85]);
assert_isequal(bitshift([255 1], 4, 'uint8'), [240 16]);
%=============================================================================
% signed assumedtype wraps within the type width
assert_isequal(bitshift(64, 1, 'int8'), -128);
assert_isequal(bitshift(127, 1, 'int8'), -2);
assert_isequal(bitshift(1, 7, 'int8'), -128);
assert_isequal(bitshift(-5, 1, 'int8'), -10);
assert_isequal(bitshift(-64, -1, 'int8'), -32);
%=============================================================================
% a typed integer input keeps its own class; a matching assumedtype is redundant
assert_isequal(bitshift(uint8(255), 4, 'uint8'), uint8(240));
assert_isequal(bitshift(int8(-1), 1, 'int8'), int8(-2));
%=============================================================================
% bitcmp with assumedtype (double input -> double result)
assert_isequal(bitcmp(5, 'uint8'), 250);
assert_isequal(class(bitcmp(5, 'uint8')), 'double');
assert_isequal(bitcmp(5, 'int8'), -6);
assert_isequal(bitcmp(uint8(5), 'uint8'), uint8(250));
%=============================================================================
% bitand / bitor / bitxor with assumedtype (double input -> double result)
assert_isequal(bitand(12, 10, 'uint8'), 8);
assert_isequal(bitor(12, 10, 'uint8'), 14);
assert_isequal(bitxor(12, 10, 'uint8'), 6);
assert_isequal(class(bitand(12, 10, 'uint8')), 'double');
%=============================================================================
% bitget / bitset with assumedtype (double input -> double result)
assert_isequal(bitget(255, 8, 'uint8'), 1);
assert_isequal(bitset(0, 8, 'uint8'), 128);
assert_isequal(class(bitget(255, 8, 'uint8')), 'double');
assert_isequal(class(bitset(0, 8, 'uint8')), 'double');
%=============================================================================
% error cases
assert_checkerror('bitshift(255, 4, ''foo'')', _('ASSUMEDTYPE must be an integer type name.'));
assert_checkerror('bitshift(uint8(255), 4, ''uint16'')', _('Integer inputs cannot be assumed as any other types than their own types.'));
assert_checkerror('bitshift(128, -1, ''int8'')', _('Double inputs must have integer values in the range of ASSUMEDTYPE.'));
assert_checkerror('bitshift(2.5, 1, ''uint8'')', _('Double inputs must have integer values in the range of ASSUMEDTYPE.'));
assert_checkerror('bitcmp(300, ''uint8'')', _('Double inputs must have integer values in the range of ASSUMEDTYPE.'));
assert_checkerror('bitcmp(uint8(5), ''uint16'')', _('Integer inputs cannot be assumed as any other types than their own types.'));
%=============================================================================
