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
p = str2func('error');
pool = backgroundPool();
f = parfeval(pool, p, 0, 'test');
wait(f);
asserts.isequal(f.State, 'finished');
%=============================================================================
p = str2func('error');
pool = backgroundPool();
f = parfeval(pool, p, 0, 1);
wait(f);
asserts.isequal(f.State, 'finished');
msg = _('The message must be specified as either a character vector, string scalar, or a message structure.');
asserts.isequal(f.Error.message, msg);
%=============================================================================
p = str2func('@(x) x +1');
pool = backgroundPool();
f = parfeval(pool, p, 1, 1);
wait(f);
asserts.isequal(f.State, 'finished');
asserts.isequal(func2str(f.Function), '@(x)x+1');
%=============================================================================
p = str2func('pause');
b = backgroundPool();
fevalqueue = b.FevalQueue;
cleanupQueue = onCleanup(@() cancelAll(fevalqueue)); %#ok<NLS0004>
NumWorkers = b.NumWorkers;
for k = [1:(NumWorkers * 2) + 2]
  f(k) = parfeval(b, p, 0, 1);
end
R = {f.State};
asserts.istrue(iscellstr(R));
asserts.isequal(length(R), (NumWorkers * 2) + 2);
%=============================================================================
fevalqueue = b.FevalQueue;
cancelAll(fevalqueue);
%=============================================================================
