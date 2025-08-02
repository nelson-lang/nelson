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
R = pyrun("", 'A', 'A', [1 2 3]);
assert_isequal(R.char(),  'array(''d'', [1.0, 2.0, 3.0])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', single([1 2 3]));
assert_isequal(R.char(),  'array(''f'', [1.0, 2.0, 3.0])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', int8([1 2 3]));
assert_isequal(R.char(),  'array(''b'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', int16([1 2 3]));
assert_isequal(R.char(),  'array(''h'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', int32([1 2 3]));
assert_isequal(R.char(),  'array(''i'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', int64([1 2 3]));
assert_isequal(R.char(),  'array(''q'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', uint8([1 2 3]));
assert_isequal(R.char(),  'array(''B'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', uint16([1 2 3]));
assert_isequal(R.char(),  'array(''H'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', uint32([1 2 3]));
assert_isequal(R.char(),  'array(''I'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', uint64([1 2 3]));
assert_isequal(R.char(),  'array(''Q'', [1, 2, 3])');
assert_isequal(R.double(), [1 2 3])
assert_isequal(R.single(), single([1 2 3]))
assert_isequal(R.int8(), int8([1 2 3]))
assert_isequal(R.int16(), int16([1 2 3]))
assert_isequal(R.int32(), int32([1 2 3]))
assert_isequal(R.int64(), int64([1 2 3]))
assert_isequal(R.uint8(), uint8([1 2 3]))
assert_isequal(R.uint16(), uint16([1 2 3]))
assert_isequal(R.uint32(), uint32([1 2 3]))
assert_isequal(R.uint64(), uint64([1 2 3]))
%=============================================================================
R = pyrun("", 'A', 'A', []);
assert_isequal(R.char, 'array(''d'')')
assert_isequal(R.double(), zeros(1, 0))
assert_isequal(R.single(), single(zeros(1, 0)))
assert_isequal(R.int8(), int8(zeros(1, 0)))
assert_isequal(R.int16(), int16(zeros(1, 0)))
assert_isequal(R.int32(), int32(zeros(1, 0)))
assert_isequal(R.int64(), int64(zeros(1, 0)))
assert_isequal(R.uint8(), uint8(zeros(1, 0)))
assert_isequal(R.uint16(), uint16(zeros(1, 0)))
assert_isequal(R.uint32(), uint32(zeros(1, 0)))
assert_isequal(R.uint64(), uint64(zeros(1, 0)))
%=============================================================================
R = pyrun("", 'A', 'A', single([]));
assert_isequal(R.char, 'array(''f'')')
assert_isequal(R.double(), zeros(1, 0))
assert_isequal(R.single(), single(zeros(1, 0)))
assert_isequal(R.int8(), int8(zeros(1, 0)))
assert_isequal(R.int16(), int16(zeros(1, 0)))
assert_isequal(R.int32(), int32(zeros(1, 0)))
assert_isequal(R.int64(), int64(zeros(1, 0)))
assert_isequal(R.uint8(), uint8(zeros(1, 0)))
assert_isequal(R.uint16(), uint16(zeros(1, 0)))
assert_isequal(R.uint32(), uint32(zeros(1, 0)))
assert_isequal(R.uint64(), uint64(zeros(1, 0)))
%=============================================================================
