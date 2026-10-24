%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Situational benchmark: a device-resident chain of element-wise operations vs
% the CPU. Only the inputs are uploaded and only the final result is gathered,
% so the transfer cost is amortized over the whole chain (the case where the
% GPU is most competitive). Performance only; correctness is in the tests.
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
sizes = [256, 512, 1024, 2048, 4096];
fprintf('%8s %16s %16s %10s\n', 'N', 'cpu chain', 'gpu chain', 'speedup');
for n = sizes
  a = single(rand(n, n));
  b = single(rand(n, n));
  % CPU: sqrt(a.^2 + b.^2) .* exp(-a) + sin(b)
  tic;
  rc = sqrt(a .^ 2 + b .^ 2) .* exp(-a) + sin(b);
  t_cpu = toc;
  % GPU: same chain, kept resident; one upload, one gather
  tic;
  A = gpuArray(a);
  B = gpuArray(b);
  R = sqrt(A .^ 2 + B .^ 2) .* exp(-A) + sin(B);
  rg = gather(R);
  t_gpu = toc;
  fprintf('%8d %14.6fs %14.6fs %9.2fx\n', n, t_cpu, t_gpu, t_cpu / t_gpu);
end
%=============================================================================
