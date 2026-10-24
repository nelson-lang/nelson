%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
R = pyrun('prime_numbers = [2, 3, 5, 7]; byte_array = bytearray(prime_numbers)', "byte_array");
asserts.isequal(class(R), 'py.bytearray');
asserts.isequal(R.int8(), int8([2, 3, 5, 7]));
asserts.isequal(R.int16(), int16([2, 3, 5, 7]));
asserts.isequal(R.int32(), int32([2, 3, 5, 7]));
asserts.isequal(R.int64(), int64([2, 3, 5, 7]));
asserts.isequal(R.uint8(), uint8([2, 3, 5, 7]));
asserts.isequal(R.uint16(), uint16([2, 3, 5, 7]));
asserts.isequal(R.uint32(), uint32([2, 3, 5, 7]));
asserts.isequal(R.uint64(), uint64([2, 3, 5, 7]));
asserts.isequal(R.single(), single([2, 3, 5, 7]));
asserts.isequal(R.double(), double([2, 3, 5, 7]));
asserts.isequal(R.numeric(), uint8([2, 3, 5, 7]));
asserts.isequal(R.char(), 'bytearray(b''\x02\x03\x05\x07'')');
asserts.checkerror('R.struct()', sprintf(_('Invalid property: %s.'), 'struct'));
%=============================================================================