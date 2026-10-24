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
n = min(15, pool.NumWorkers);
for i = 1:n
  f(i) = parfeval(pool, fptr, 0, 1);
end
tic()
R = wait(f, 'running');
toc()
asserts.istrue(R);
wait(f, 'finished', 10);
%=============================================================================
