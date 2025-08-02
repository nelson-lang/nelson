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
b = backgroundPool();
NumWorkers = b.NumWorkers;
for k = [1:(NumWorkers*2) + 2]
  f(k) = parfeval(b, p, 0, 5);
end
fToStop = f(NumWorkers + 2); 
assert_isequal(fToStop.State, 'queued');
cancel(fToStop);
assert_isequal(fToStop.State, 'finished');
assert_isequal(class(fToStop.Error), 'MException');
cancel(f);