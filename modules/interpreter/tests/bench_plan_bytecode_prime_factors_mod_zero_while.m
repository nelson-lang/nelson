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
n0 = 160001;
d = [];
n = n0;
p = 2;
tic();
while n ~= 1
  while mod(n, p) == 0
    d = [d, p];
    n = n / p;
  end
  p = p + 1;
end
elapsed = toc();
ops = n0;
asserts.istrue(isempty(d) || d(1) > 0);
fprintf('BENCH_REFERENCE,scalar,bytecode prime factors mod zero while,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
