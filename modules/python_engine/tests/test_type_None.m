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
asserts.isequal(class(R), 'py.NoneType');
asserts.isequal(R.char, 'None');
asserts.istrue(contains(evalc('R'), 'Python NoneType'));
asserts.istrue(contains(evalc('disp(R)'), 'Python NoneType'));
asserts.isequal(strtrim(evalc('py.print(''f'')')), 'f');
asserts.isequal(strtrim(evalc('py.print(''f'');')), 'f');
asserts.checkerror('R.struct()', sprintf(_('Invalid property: %s.'), 'struct'));
asserts.checkerror('R.cell()', sprintf(_('Invalid property: %s.'), 'cell'));
asserts.checkerror('R.int8()', sprintf(_('Invalid property: %s.'), 'int8'));
asserts.checkerror('R.int16()', sprintf(_('Invalid property: %s.'), 'int16'));
asserts.checkerror('R.int32()', sprintf(_('Invalid property: %s.'), 'int32'));
asserts.checkerror('R.int64()', sprintf(_('Invalid property: %s.'), 'int64'));
asserts.checkerror('R.uint8()', sprintf(_('Invalid property: %s.'), 'uint8'));
asserts.checkerror('R.uint16()', sprintf(_('Invalid property: %s.'), 'uint16'));
asserts.checkerror('R.uint32()', sprintf(_('Invalid property: %s.'), 'uint32'));
asserts.checkerror('R.uint64()', sprintf(_('Invalid property: %s.'), 'uint64'));
asserts.checkerror('R.single()', sprintf(_('Invalid property: %s.'), 'single'));
asserts.checkerror('R.double()', sprintf(_('Invalid property: %s.'), 'double'));
