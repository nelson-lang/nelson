%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Situational benchmark: GPU (WebGPU) vs CPU on the column reductions sum and
% cumsum in single precision. Performance only; correctness is in the tests.
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
sizes = [256, 512, 1024, 2048, 4096];
fprintf('%8s %14s %14s %14s %14s\n', 'N', 'cpu sum', 'gpu sum', 'cpu cumsum', 'gpu cumsum');
for n = sizes
  a = single(rand(n, n));
  tic; s1 = sum(a); t_cpu_sum = toc;
  tic; c1 = cumsum(a); t_cpu_cumsum = toc;
  % GPU: upload + compute + readback (full round trip)
  tic;
  A = gpuArray(a);
  S = sum(A);
  rs = gather(S);
  t_gpu_sum = toc;
  tic;
  C = cumsum(A);
  rc = gather(C);
  t_gpu_cumsum = toc;
  fprintf('%8d %12.6fs %12.6fs %12.6fs %12.6fs\n', ...
    n, t_cpu_sum, t_gpu_sum, t_cpu_cumsum, t_gpu_cumsum);
end
%=============================================================================
