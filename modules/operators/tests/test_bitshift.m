%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% left shift
assert_isequal(bitshift(uint8(1), 3), uint8(8));
assert_isequal(bitshift(int32(1), 4), int32(16));
assert_isequal(bitshift(5, 2), 20);
%=============================================================================
% right shift (arithmetic for signed, logical for unsigned)
assert_isequal(bitshift(int32(-8), -1), int32(-4));
assert_isequal(bitshift(uint8(255), -4), uint8(15));
%=============================================================================
% left shift truncates to the class width (wraps, does not saturate)
assert_isequal(bitshift(uint8(64), 2), uint8(0));
assert_isequal(bitshift(int8(64), 2), int8(0));
assert_isequal(bitshift(int32(1073741824), 2), int32(0));
%=============================================================================
% class is preserved; a shift larger than the width clears the value
assert_isequal(class(bitshift(int16(3), 1)), 'int16');
assert_isequal(bitshift(uint16(1), 20), uint16(0));
%=============================================================================
% array value and array shift (element-wise, and scalar broadcast)
assert_isequal(bitshift(uint8([1 2 4]), [1 2 3]), uint8([2 8 32]));
assert_isequal(bitshift(uint8([1 2 4]), 1), uint8([2 4 8]));
%=============================================================================
% errors
assert_checkerror('bitshift(2.5, 1)', _('bitshift: finite integer values expected.'));
%=============================================================================
