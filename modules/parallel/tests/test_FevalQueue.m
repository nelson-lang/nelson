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
T = maxNumCompThreads();
cleanupThreads = onCleanup(@() maxNumCompThreads(T)); %#ok<NLS0004>
if (T > 2)
  maxNumCompThreads(2);
end
%=============================================================================
p = str2func('pause');
b = backgroundPool();
NumWorkers = b.NumWorkers;
asserts.isequal(NumWorkers, maxNumCompThreads());
totalWorkers = (NumWorkers * 2) + 2;
for k = 1:totalWorkers
  f(k) = parfeval(b, p, 0, 3);
end
%=============================================================================
asserts.isequal(class(f), 'FevalFuture');
%=============================================================================
wait(f(1), 'running');
K = b.FevalQueue;
% wait for both workers to pick up a task before sampling the running/queued split
for attempt = 1:100
  if (length(K.RunningFutures) >= NumWorkers)
    break;
  end
  sleep(0.05);
end
R1 = K.RunningFutures;
LEN_R1 = length(R1);
R2 = K.QueuedFutures;
LEN_R2 = length(R2);
%=============================================================================
asserts.isequal(length(f), totalWorkers);
asserts.isequal(LEN_R1 + LEN_R2, totalWorkers);
%=============================================================================
asserts.isequal(size(K), [1 1]);
%=============================================================================
asserts.isequal(LEN_R1, NumWorkers);
%=============================================================================
asserts.isequal(LEN_R2, NumWorkers + 2);
wait(f, 'finished', 10);
%=============================================================================
