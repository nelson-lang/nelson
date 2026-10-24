%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Situational benchmark: a multi-op elementwise expression evaluated three
% ways, in single precision. Performance only; correctness is in the tests.
%   expr = sqrt(x.^2 + y.^2)   (4 elementwise ops)
%   - cpu        : host vectorized
%   - gpu ops    : separate gpuArray operators (several kernels + temporaries)
%   - gpu fused  : arrayfun compiled into a single fused WGSL kernel
% A and B are uploaded once (not timed); each column times compute + gather.
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
f = @(x, y) sqrt(x .^ 2 + y .^ 2);
sizes = [256, 512, 1024, 2048, 4096];
fprintf('%8s %14s %14s %14s\n', 'N', 'cpu', 'gpu ops', 'gpu fused');
for n = sizes
  a = single(rand(n, n));
  b = single(rand(n, n));
  A = gpuArray(a);
  B = gpuArray(b);
  % warm-up (kernel/pipeline compilation is cached on first use)
  w1 = gather(sqrt(A .^ 2 + B .^ 2));
  w2 = gather(arrayfun(f, A, B));
  % CPU
  tic; rc = f(a, b); t_cpu = toc;
  % GPU, separate operators (each op is its own kernel + intermediate buffer)
  tic; Rs = sqrt(A .^ 2 + B .^ 2); rs = gather(Rs); t_ops = toc;
  % GPU, single fused kernel
  tic; Rf = arrayfun(f, A, B); rf = gather(Rf); t_fused = toc;
  fprintf('%8d %12.6fs %12.6fs %12.6fs\n', n, t_cpu, t_ops, t_fused);
end
%=============================================================================
