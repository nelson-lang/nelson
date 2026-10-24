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
pool = backgroundPool();
f = parfeval(pool, fptr, 0, 2);
tic()
R = wait(f, 'finished', 1);
T = toc();
asserts.isequal(f.State, 'running');
asserts.isfalse(R);
asserts.istrue(abs(T - 1) < 1);
wait(f, 'finished', 3);
%=============================================================================
