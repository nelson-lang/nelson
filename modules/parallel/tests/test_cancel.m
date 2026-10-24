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
b = backgroundPool();
fevalqueue = b.FevalQueue;
cleanupQueue = onCleanup(@() cancelAll(fevalqueue)); %#ok<NLS0004>
NumWorkers = b.NumWorkers;
for k = 1:((NumWorkers * 2) + 2)
  f(k) = parfeval(b, p, 0, 1);
end
fToStop = f(NumWorkers + 2);
asserts.isequal(fToStop.State, 'queued');
cancel(fToStop);
asserts.isequal(fToStop.State, 'finished');
asserts.isequal(class(fToStop.Error), 'MException');
cancel(f);
wait(f, 'finished', 3);
cancelAll(fevalqueue);
