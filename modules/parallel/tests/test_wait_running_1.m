%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
fptr = str2func('cos');
f = parfeval(backgroundPool, fptr, 0, 55);
pause(2);
R = wait(f, 'running');
assert_isequal(f.State, 'finished');
assert_istrue(R);
%=============================================================================
