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
for k = [1:(NumWorkers*2) + 2]
    f(k) = parfeval(b, p, 0, 50);
end
K =  b.FevalQueue;
R1 = K.RunningFutures;
R2 = K.QueuedFutures;
%=============================================================================
sleep(1); 
%=============================================================================
assert_isequal(size(R1), [1 , NumWorkers ]);
assert_isequal(size(R2), [1 , (NumWorkers + 2) ]);
%=============================================================================
