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
assert_isequal(nargin('fetchNext'), 1);
assert_isequal(nargout('fetchNext'), 1);
%=============================================================================
pool = backgroundPool();
N = pool.NumWorkers;
%=============================================================================
for idx = 1:N
  f(idx) = parfeval(backgroundPool, str2func('pause'), 0, idx); 
end
%=============================================================================
for idx = 1:N
  tic(), dd = fetchNext(f), t = toc();
  assert_istrue(t > 0 && t <= 3);
  assert_isequal(dd, idx);
end
%=============================================================================
assert_checkerror('dd = fetchNext(f)', _('There are no unread Futures to fetch.'));
%=============================================================================
l = parfeval(backgroundPool, str2func('pause'), 0, inf); 
tic(); dd = fetchNext(l, 5); t = toc();
assert_istrue(t > 0 && t <= 6);
assert_isequal(dd, []);
%=============================================================================
