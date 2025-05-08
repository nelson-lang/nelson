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
p = str2func('error');
pool = backgroundPool();
f = parfeval(pool, p, 0, 'test');
wait(f);
assert_isequal(f.State, 'finished');
%=============================================================================
p = str2func('error');
pool = backgroundPool();
f = parfeval(pool, p, 0, 1);
wait(f);
assert_isequal(f.State, 'finished');
assert_isequal(f.Error.message, _('Wrong type for argument #1: string expected.'));
%=============================================================================
p = str2func('@(x) x +1');
pool = backgroundPool();
f = parfeval(pool, p, 1, 1);
wait(f);
assert_isequal(f.State, 'finished');
assert_isequal(func2str(f.Function), '@(x) x+1');
%=============================================================================
p = str2func('pause');
b = backgroundPool();
NumWorkers = b.NumWorkers;
for k = [1:(NumWorkers*2) + 2]
  f(k) = parfeval(b, p, 0, 5);
end
R = {f.State};
assert_istrue(iscellstr(R));
assert_isequal(length(R), (NumWorkers*2) + 2);
%=============================================================================
fevalqueue = b.FevalQueue;
cancelAll(fevalqueue);
%=============================================================================
