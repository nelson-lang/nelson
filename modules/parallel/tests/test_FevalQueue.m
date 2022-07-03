%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = str2func('pause');
b = backgroundPool();
NumWorkers = b.NumWorkers;
totalWorkers = (NumWorkers*2) + 2
for k = 1:totalWorkers
    f(k) = parfeval(b, p, 0, 50);
end
%=============================================================================
assert_isequal(class(f), 'FevalFuture')
assert_isequal(length(f), totalWorkers);
%=============================================================================
K =  b.FevalQueue;
assert_isequal(size(K), [1 1]);
%=============================================================================
R1 = K.RunningFutures;
assert_isequal(length(R1), NumWorkers);
%=============================================================================
R2 = K.QueuedFutures;
assert_isequal(length(R2), NumWorkers + 2);
%=============================================================================
