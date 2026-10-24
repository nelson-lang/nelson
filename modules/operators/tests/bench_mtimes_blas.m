%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% <--INDEX 64 BIT REQUIRED-->
%=============================================================================
% Bench: a dense double matrix product routes through the threaded BLAS xgemm
% (MKL / OpenBLAS) with every core, not Eigen's single-threaded kernel.
% A 2000x2000 product (1.6e10 flops) runs in ~40 ms threaded here; Eigen's
% built-in kernel takes ~300 ms. A regression to the built-in kernel or to a
% thread-starved backend trips the 200 ms bound even on a modest multicore.
% Correctness of the BLAS path lives in test_mtimes.
%=============================================================================
N = 2000;
X = rand(N, N);
Y = rand(N, N);
Z = X * Y; % warm the BLAS resolve
best = inf;
for r = 1:3
  tic();
  Z = X * Y;
  best = min(best, toc());
end
fprintf('  mtimes %dx%d: %0.1f ms\n', N, N, best * 1000);
asserts.istrue(best < 0.2);
%=============================================================================
