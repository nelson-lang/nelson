%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
n = 48000;
a = 160001;
p = 2;
hits = 0;
tic();
for k = 1:n
  if mod(a, p) == 0
    hits = hits + 1;
  end
  p = p + 1;
  if p > 997
    p = 2;
  end
end
elapsed = toc();
ops = n;
asserts.istrue(hits >= 0);
fprintf('BENCH_REFERENCE,numeric,local scalar mod zero branch,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
