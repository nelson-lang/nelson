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
for i = 1:15
  f(i) = parfeval(pool, fptr, 0, 5);
end
tic()
R = wait(f, 'finished');
toc()
asserts.istrue(R);
%=============================================================================
