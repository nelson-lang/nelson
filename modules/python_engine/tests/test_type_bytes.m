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
R = pyrun("message = 'Python is fun';byte_message = bytes(message, 'utf-8')", "byte_message");
%=============================================================================
REF = [80   121   116   104   111   110    32   105   115    32   102   117   110];
%=============================================================================
assert_isequal(class(R), 'py.bytes');
assert_isequal(double(R), REF);
assert_isequal(R.double, REF);
%=============================================================================
assert_isequal(R.char(), 'b''Python is fun''');
assert_isequal(R.int8(), int8(REF));
assert_isequal(R.int16(), int16(REF));
assert_isequal(R.int32(), int32(REF));
assert_isequal(R.int64(), int64(REF));
assert_isequal(R.uint8(), uint8(REF));
assert_isequal(R.uint16(), uint16(REF));
assert_isequal(R.uint32(), uint32(REF));
assert_isequal(R.uint64(), uint64(REF));
assert_isequal(R.single(), single(REF));
assert_checkerror('R.struct()', _('Wrong value for #2 argument. struct'));
assert_checkerror('R.string()', _('Wrong value for #2 argument. string'));
%=============================================================================

