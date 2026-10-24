%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
% <--TIMEOUT 30-->
skip_testsuite(~ismodule('parallel'), 'Parallel module unavailable');
previousThreads = maxNumCompThreads();
skip_testsuite(previousThreads < 2, 'Two workers are required');
cleanupThreads = onCleanup(@() maxNumCompThreads(previousThreads));
maxNumCompThreads(2);
testPath = [modulepath('interpreter'), '/tests'];
addpath(testPath);
cleanupPath = onCleanup(@() rmpath(testPath));
future = parfeval(backgroundPool(), @scalar_range_loop_values, 4, 1, 1, 1e12, 0, 1);
cleanupFuture = onCleanup(@() cancel(future));
wait(future, 'running', 5);
asserts.isequal(future.State, 'running');
pause(0.02);
cancel(future);
wait(future, 'finished', 5);
asserts.isequal(future.State, 'finished');
asserts.isequal(class(future.Error), 'MException');
