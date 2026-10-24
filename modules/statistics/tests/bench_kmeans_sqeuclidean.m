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
start = [zeros(1, 6); ones(1, 6) * 3; ones(1, 6) * 6; ones(1, 6) * 9];
opts = statset('UseParallel', false);
[idxReference, CReference, sumdReference] = kmeans(X, 4, 'Start', start, ...
  'MaxIter', 20, 'OnlinePhase', 'off', 'Options', opts);
times = zeros(1, 5);
for k = 1:5
  tic();
  [idx, C, sumd] = kmeans(X, 4, 'Start', start, 'MaxIter', 20, ...
    'OnlinePhase', 'off', 'Options', opts);
  times(k) = toc();
  asserts.isequal(idx, idxReference);
  asserts.isequal(C, CReference);
  asserts.isequal(sumd, sumdReference);
end
disp(['kmeans sqeuclidean observations: ', int2str(size(X, 1))]);
disp(['kmeans sqeuclidean best time: ', num2str(min(times)), ' s']);
disp(['kmeans sqeuclidean median time: ', num2str(median(times)), ' s']);
%=============================================================================
