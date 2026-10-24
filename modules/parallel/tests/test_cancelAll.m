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
p = str2func('pause');
previousNumThreads = maxNumCompThreads();
cleanupThreads = onCleanup(@() maxNumCompThreads(previousNumThreads)); %#ok<NLS0004>
if previousNumThreads > 2
  maxNumCompThreads(2);
end
pool = backgroundPool();
fevalqueue = pool.FevalQueue;
cleanupQueue = onCleanup(@() cancelAll(fevalqueue)); %#ok<NLS0004>
NumWorkers = pool.NumWorkers;
for k = 1:((NumWorkers * 2) + 2)
  f(k) = parfeval(pool, p, 0, 1);
end
asserts.isequal(length(f), (NumWorkers * 2) + 2);
asserts.isequal(f(end).State, 'queued');
cancelAll(fevalqueue);
asserts.isequal(f(end).State, 'finished');
asserts.istrue(isempty(fevalqueue.QueuedFutures));
asserts.istrue(isempty(fevalqueue.RunningFutures));
cancelAll(fevalqueue);
