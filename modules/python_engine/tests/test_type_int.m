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
asserts.isequal(class(R), 'py.int');
asserts.isequal(R.char, '3');
asserts.isequal(R.int8(), int8(3));
asserts.isequal(R.int16(), int16(3));
asserts.isequal(R.int32(), int32(3));
asserts.isequal(R.int64(), int64(3));
asserts.isequal(R.uint8(), uint8(3));
asserts.isequal(R.uint16(), uint16(3));
asserts.isequal(R.uint32(), uint32(3));
asserts.isequal(R.uint64(), uint64(3));
asserts.isequal(R.single(), single(3));
asserts.isequal(R.double(), double(3));
%=============================================================================
asserts.checkerror('R.cell()', sprintf(_('Invalid property: %s.'), 'cell'));
asserts.checkerror('R.struct()', sprintf(_('Invalid property: %s.'), 'struct'));
asserts.checkerror('R.numeric()', sprintf(_('Invalid property: %s.'), 'numeric'));
%=============================================================================
