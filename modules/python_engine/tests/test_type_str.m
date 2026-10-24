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
R = pyrun("R = 'Hello from Python'", "R");
asserts.isequal(R.char, 'Hello from Python');
asserts.isequal(R.string, "Hello from Python");
asserts.checkerror('R.struct()', sprintf(_('Invalid property: %s.'), 'struct'));
TXT = evalc('R');
asserts.istrue(contains(TXT, 'Python str with no properties.'));
asserts.istrue(contains(TXT, '    Hello from Python'));
asserts.isfalse(contains(TXT, '''Hello from Python'''));
%=============================================================================
