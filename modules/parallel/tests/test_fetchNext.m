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
asserts.isequal(nargin('fetchNext'), 1);
asserts.isequal(nargout('fetchNext'), 1);
%=============================================================================
pool = backgroundPool();
N = pool.NumWorkers;
%=============================================================================
for idx = 1:N
  f(idx) = parfeval(pool, str2func('pause'), 0, idx);
end
%=============================================================================
for idx = 1:N
  tic(), dd = fetchNext(f), t = toc();
  asserts.istrue(t > 0 && t <= 3);
  asserts.isequal(dd, idx);
end
%=============================================================================
asserts.checkerror('dd = fetchNext(f)', _('There are no unread Futures to fetch.'));
%=============================================================================
l = parfeval(pool, str2func('pause'), 0, 6);
tic(); dd = fetchNext(l, 5); t = toc();
asserts.istrue(t > 0 && t <= 6);
asserts.isequal(dd, []);
cancel(l);
wait(l, 'finished', 2);
%=============================================================================
