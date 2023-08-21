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
T = maxNumCompThreads();
if (T > 2)
  maxNumCompThreads(2);
end
%=============================================================================
p = str2func('pause');
b = backgroundPool();
NumWorkers = b.NumWorkers;
totalWorkers = (NumWorkers*2) + 2
for k = 1:totalWorkers
  f(k) = parfeval(b, p, 0, Inf);
end
%=============================================================================
assert_isequal(class(f), 'FevalFuture')
%=============================================================================
wait(f(1), 'running');
K =  b.FevalQueue;
R1 = K.RunningFutures;
LEN_R1 = length(R1);
R2 = K.QueuedFutures;
LEN_R2 = length(R2);
%=============================================================================
assert_isequal(length(f), totalWorkers);
assert_isequal(LEN_R1 + LEN_R2, totalWorkers);
%=============================================================================
assert_isequal(size(K), [1 1]);
%=============================================================================
assert_isequal(LEN_R1, NumWorkers);
%=============================================================================
assert_isequal(LEN_R2, NumWorkers + 2);
%=============================================================================
