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
X = [rand(n, 2); rand(n, 2) + 3; rand(n, 2) + 6; rand(n, 2) + 9];
start = [0 0; 3 3; 6 6; 9 9];
[idxReference, CReference, sumdReference] = kmeans(X, 4, 'Start', start, ...
  'Distance', 'cityblock', 'MaxIter', 20, 'OnlinePhase', 'off');
times = zeros(1, 5);
for k = 1:5
  tic();
  [idx, C, sumd] = kmeans(X, 4, 'Start', start, 'Distance', 'cityblock', ...
    'MaxIter', 20, 'OnlinePhase', 'off');
  times(k) = toc();
  asserts.isequal(idx, idxReference);
  asserts.isequal(C, CReference);
  asserts.isequal(sumd, sumdReference);
end
disp(['kmeans cityblock observations: ', int2str(size(X, 1))]);
disp(['kmeans cityblock best time: ', num2str(min(times)), ' s']);
disp(['kmeans cityblock median time: ', num2str(median(times)), ' s']);
%=============================================================================
