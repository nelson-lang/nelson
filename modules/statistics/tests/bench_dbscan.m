%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
% <--REUSE PROCESS-->
%=============================================================================
rng(0);
n = 3000;
X = [rand(n, 8); rand(n, 8) + 3; rand(n, 8) + 6];
epsilon = 0.65;
minpts = 5;
idxReference = dbscan(X, epsilon, minpts);
idx = dbscan(X, epsilon, minpts);
asserts.isequal(idx, idxReference);
times = zeros(1, 5);
for k = 1:5
  tic();
  idx = dbscan(X, epsilon, minpts);
  times(k) = toc();
  asserts.isequal(idx, idxReference);
end
disp(['dbscan observations: ', int2str(size(X, 1))]);
disp(['dbscan best time: ', num2str(min(times)), ' s']);
disp(['dbscan median time: ', num2str(median(times)), ' s']);
disp(['dbscan mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
