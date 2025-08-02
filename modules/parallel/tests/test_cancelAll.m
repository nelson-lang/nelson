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
pool = backgroundPool();
NumWorkers = pool.NumWorkers;
for k = [1:(NumWorkers*2) + 2]
  f(k) = parfeval(pool, p, 0, 5);
end
assert_isequal(length(f), (NumWorkers*2) + 2);
assert_isequal(f(end).State, 'queued')
fevalqueue = pool.FevalQueue;
cancelAll(fevalqueue);
assert_isequal(f(end).State, 'finished')
assert_istrue(isempty(fevalqueue.QueuedFutures))
assert_istrue(isempty(fevalqueue.RunningFutures))
