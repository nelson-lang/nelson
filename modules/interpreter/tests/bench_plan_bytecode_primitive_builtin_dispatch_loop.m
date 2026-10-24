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
n = 8000;
x = linspace(0.001, 0.999, n);
value = 0;
tic();
for k = 1:n
  z = x(k);
  value = abs(-z) + sin(z) + cos(z) + tan(z) + exp(z) + log(z) + log2(z) + log10(z) + gamma(z) + atan(z) + floor(z) + ceil(z) + fix(-z) + round(z) + sqrt(z);
end
elapsed = toc();
ops = n * 15;
asserts.istrue(value > 0);
fprintf('BENCH_REFERENCE,call,bytecode primitive builtin dispatch loop,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
