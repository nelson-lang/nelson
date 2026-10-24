%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Situational benchmark: GPU (WebGPU) vs CPU on the transcendental element-wise
% functions sqrt/exp/sin in single precision (full round trip on the GPU).
% Performance only; correctness is in the tests.
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
sizes = [256, 512, 1024, 2048, 4096];
fprintf('%8s %14s %14s %14s %14s %14s %14s\n', ...
  'N', 'cpu sqrt', 'gpu sqrt', 'cpu exp', 'gpu exp', 'cpu sin', 'gpu sin');
for n = sizes
  a = single(rand(n, n));
  tic; s1 = sqrt(a); t_cpu_sqrt = toc;
  tic; e1 = exp(a); t_cpu_exp = toc;
  tic; n1 = sin(a); t_cpu_sin = toc;
  A = gpuArray(a);
  tic; rs = gather(sqrt(A)); t_gpu_sqrt = toc;
  tic; re = gather(exp(A)); t_gpu_exp = toc;
  tic; rn = gather(sin(A)); t_gpu_sin = toc;
  fprintf('%8d %12.6fs %12.6fs %12.6fs %12.6fs %12.6fs %12.6fs\n', ...
    n, t_cpu_sqrt, t_gpu_sqrt, t_cpu_exp, t_gpu_exp, t_cpu_sin, t_gpu_sin);
end
%=============================================================================
