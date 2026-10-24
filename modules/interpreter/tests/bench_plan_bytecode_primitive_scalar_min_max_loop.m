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
value = 0;
tic();
for k = 1:n
  a = 56 - k + floor(k / 56) * 56;
  b = 24;
  value = value + min(a, b) + max(a, b);
end
elapsed = toc();
ops = n * 2;
asserts.istrue(value > 0);
fprintf('BENCH_REFERENCE,numeric,primitive scalar min max loop,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
