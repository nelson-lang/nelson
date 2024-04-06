%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
R = pyrun("", "P",'P', [1 2; 3 4]);
REF = [1 2; 3 4];
assert_isequal(R.double, REF);
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
R = pyrun("",  'A', 'A', [1 2; 3 4]);
assert_isequal(class(R), 'py.memoryview') 
assert_isequal(R.format.char, 'd');
assert_isequal(R.double, [1 2; 3 4])
%=============================================================================
R = pyrun("",  'A', 'A', single([1 2; 3 4]));
assert_isequal(class(R), 'py.memoryview') 
assert_isequal(R.format.char, 'f');
assert_isequal(R.double, [1 2; 3 4])
%=============================================================================

R = pyrun("",  'A', 'A', [1 2 3]+i);
assert_isequal(class(R), 'py.memoryview') 
assert_isequal(R.format.char, 'Zd');
assert_isequal(R.double, [1 2 3]+i)
%=============================================================================
R = pyrun("", 'A', 'A', single([1 2 3]+i));
assert_isequal(class(R), 'py.memoryview') 
assert_isequal(R.format.char, 'Zf');
assert_isequal(R.double, [1 2 3]+i)
%=============================================================================
A = int8([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
A = int16([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
A = int32([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
A = int64([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
A = uint8([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
A = uint16([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
A = uint32([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
A = uint64([1 2; 3 4]);
R = pyrun("", "P",'P', A);
REF = A;
assert_isequal(R.double, double(REF));
assert_isequal(R.single, single(REF));
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_checkerror('R.cell()', _('Cannot convert to cell'));
%=============================================================================
