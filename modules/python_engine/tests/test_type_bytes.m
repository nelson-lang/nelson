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
R = pyrun("message = 'Python is fun';byte_message = bytes(message, 'utf-8')", "byte_message");
%=============================================================================
REF = [80 121 116 104 111 110 32 105 115 32 102 117 110];
%=============================================================================
asserts.isequal(class(R), 'py.bytes');
asserts.isequal(double(R), REF);
asserts.isequal(R.double, REF);
%=============================================================================
asserts.isequal(R.char(), 'b''Python is fun''');
asserts.isequal(R.int8(), int8(REF));
asserts.isequal(R.int16(), int16(REF));
asserts.isequal(R.int32(), int32(REF));
asserts.isequal(R.int64(), int64(REF));
asserts.isequal(R.uint8(), uint8(REF));
asserts.isequal(R.uint16(), uint16(REF));
asserts.isequal(R.uint32(), uint32(REF));
asserts.isequal(R.uint64(), uint64(REF));
asserts.isequal(R.single(), single(REF));
asserts.checkerror('R.struct()', sprintf(_('Invalid property: %s.'), 'struct'));
asserts.checkerror('R.string()', sprintf(_('Invalid property: %s.'), 'string'));
%=============================================================================

