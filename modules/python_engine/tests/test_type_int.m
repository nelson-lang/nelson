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
R = pyrun('P = 3', "P");
assert_isequal(class(R), 'py.int');
assert_isequal(R.char, '3');
assert_isequal(R.int8(), int8(3));
assert_isequal(R.int16(), int16(3));
assert_isequal(R.int32(), int32(3));
assert_isequal(R.int64(), int64(3));
assert_isequal(R.uint8(), uint8(3));
assert_isequal(R.uint16(), uint16(3));
assert_isequal(R.uint32(), uint32(3));
assert_isequal(R.uint64(), uint64(3));
assert_isequal(R.single(), single(3));
assert_isequal(R.double(), double(3));
%=============================================================================
assert_checkerror('R.cell()', [_('Wrong value for #2 argument.'), ' ', 'cell']);
assert_checkerror('R.struct()', [_('Wrong value for #2 argument.'), ' ', 'struct']);
assert_checkerror('R.numeric()', [_('Wrong value for #2 argument.'), ' ', 'numeric']);
%=============================================================================
