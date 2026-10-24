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
n = 1000000;
mu = linspace(0.01, 10, n);
times = zeros(1, 5);
for k = 1:5
  rng(0);
  tic();
  x = exprnd(2, n, 1);
  [m, v] = expstat(mu);
  times(k) = toc();
  asserts.isequal(size(x), [n 1]);
  asserts.isequal(size(m), [1 n]);
  asserts.isequal(size(v), [1 n]);
end
disp(['exponential random/stat elements: ', int2str(n)]);
disp(['exponential random/stat best time: ', num2str(min(times)), ' s']);
disp(['exponential random/stat median time: ', num2str(median(times)), ' s']);
disp(['exponential random/stat mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
