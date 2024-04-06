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
R = pyrun("R = 'Hello from Python'", "R");
assert_isequal(R.char, 'Hello from Python');
assert_isequal(R.string, "Hello from Python");
assert_checkerror('R.struct()', _('Wrong value for #2 argument. struct'));
%=============================================================================
