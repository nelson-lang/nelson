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
fevalqueue = pool.FevalQueue;
cleanupQueue = onCleanup(@() cancelAll(fevalqueue)); %#ok<NLS0004>
f = parfeval(pool, fptr, 0, 2);
tic()
R = wait(f, 'running', 1);
T = toc();
asserts.isequal(f.State, 'running');
asserts.istrue(R);
R = wait(f, 'finished', 3);
asserts.istrue(R);
cancelAll(fevalqueue);
%=============================================================================
