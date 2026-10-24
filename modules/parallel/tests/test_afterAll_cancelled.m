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
previousNumThreads = maxNumCompThreads();
cleanupThreads = onCleanup(@() maxNumCompThreads(previousNumThreads)); %#ok<NLS0004>
if previousNumThreads ~= 1
  maxNumCompThreads(1);
end
pool = backgroundPool();
fptr = str2func('pause');
f(1) = parfeval(pool, fptr, 0, Inf);
f(2) = parfeval(pool, fptr, 0, Inf);
wait(f(1), 'running');
cancel(f(2));
cancel(f(1));
wait(f, 'finished', 5);
afterAllFunction = str2func('@(x) x');
c = afterAll(f, afterAllFunction, 1);
caught = false;
message = '';
identifier = '';
try
  fetchOutputs(c);
catch ex
  caught = true;
  message = ex.message;
  identifier = ex.identifier;
end
fevalqueue = pool.FevalQueue;
cancelAll(fevalqueue);
clear c f;
%=============================================================================
asserts.istrue(caught);
asserts.istrue(contains(message, _('One or more futures resulted in an error.')));
asserts.isequal(identifier, 'Nelson:parallel:future:ExecutionError');
%=============================================================================
