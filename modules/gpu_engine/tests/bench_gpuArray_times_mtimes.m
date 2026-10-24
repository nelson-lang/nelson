%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Situational benchmark: GPU (WebGPU) vs CPU on .* and * in single
% precision. Performance only, correctness is covered by the tests.
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
sizes = [256, 512, 1024, 2048, 4096];
fprintf('%8s %14s %14s %14s %14s\n', 'N', 'cpu .*', 'gpu .*', 'cpu *', 'gpu *');
for n = sizes
  a = single(rand(n, n));
  b = single(rand(n, n));
  % CPU elementwise
  tic; c1 = a .* b; t_cpu_times = toc;
  % CPU matrix product
  tic; c2 = a * b; t_cpu_mtimes = toc;
  % GPU (upload + compute + readback: measures the full round trip)
  tic;
  A = gpuArray(a);
  B = gpuArray(b);
  C1 = A .* B;
  r1 = gather(C1);
  t_gpu_times = toc;
  tic;
  C2 = A * B;
  r2 = gather(C2);
  t_gpu_mtimes = toc;
  fprintf('%8d %12.6fs %12.6fs %12.6fs %12.6fs\n', n, t_cpu_times, t_gpu_times, t_cpu_mtimes, t_gpu_mtimes);
end
%=============================================================================
