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
R = pyrun('P = None', "P");
assert_isequal(class(R), 'py.NoneType');
assert_isequal(R.char, 'None');
assert_checkerror('R.struct()', _('Wrong value for #2 argument. struct'));
assert_checkerror('R.cell()', _('Wrong value for #2 argument. cell'));
assert_checkerror('R.int8()', _('Wrong value for #2 argument. int8'));
assert_checkerror('R.int16()', _('Wrong value for #2 argument. int16'));
assert_checkerror('R.int32()', _('Wrong value for #2 argument. int32'));
assert_checkerror('R.int64()', _('Wrong value for #2 argument. int64'));
assert_checkerror('R.uint8()', _('Wrong value for #2 argument. uint8'));
assert_checkerror('R.uint16()', _('Wrong value for #2 argument. uint16'));
assert_checkerror('R.uint32()', _('Wrong value for #2 argument. uint32'));
assert_checkerror('R.uint64()', _('Wrong value for #2 argument. uint64'));
assert_checkerror('R.single()', _('Wrong value for #2 argument. single'));
assert_checkerror('R.double()', _('Wrong value for #2 argument. double'));
