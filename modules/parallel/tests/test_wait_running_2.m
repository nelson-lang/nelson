%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
fptr = str2func('pause');
f = parfeval(backgroundPool, fptr, 0, 55);
tic()
R = wait(f, 'running', 1);
T = toc();
assert_isequal(f.State, 'running');
assert_istrue(R);
%=============================================================================

