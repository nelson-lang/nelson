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
A = pyrun('','A', 'A', int32(3));
assert_isequal(A.keyHash(), uint64(3));
%=============================================================================
R = pyrun("R = {'name': 'Dionysia', 'age': 28, 'location': 'Athens'}", "R");
msg = sprintf(_("TypeError: unhashable type: '%s'"), 'dict')
assert_checkerror('R.keyHash()', msg)
%=============================================================================
