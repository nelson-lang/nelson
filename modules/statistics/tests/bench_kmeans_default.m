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
rng(0);
n = 1200;
X = [rand(n, 6); rand(n, 6) + 3; rand(n, 6) + 6; rand(n, 6) + 9];
runs = 5;
times = zeros(1, runs);
for k = 1:runs
  tic();
  [idx, C] = kmeans(X, 4);
  times(k) = toc();
  assert(isequal(size(idx), [size(X, 1) 1]));
  assert(isequal(size(C), [4 6]));
end
disp(['kmeans default observations: ', int2str(size(X, 1))]);
disp(['kmeans default best time: ', num2str(min(times)), ' s']);
disp(['kmeans default median time: ', num2str(median(times)), ' s']);
disp(['kmeans default mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
