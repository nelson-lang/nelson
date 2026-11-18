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
% Benchmark the squared Euclidean assignment engines (legacy / elkan / hamerly)
% on a large clustered dataset, where triangle-inequality pruning pays off once
% points settle. The dataset is large enough that each run is well above the
% timing noise floor. All engines must return identical results.
%=============================================================================
rng(0);
m = 30;
perCluster = 1600;
dims = 16;
X = zeros(m * perCluster, dims);
start = zeros(m, dims);
for j = 1:m
  center = (j - 1) * 6 * ones(1, dims);
  X((j - 1) * perCluster + 1:j * perCluster, :) = rand(perCluster, dims) + center;
  start(j, :) = center;
end
opts = statset('UseParallel', false);
runs = 4;
engines = {'legacy', 'elkan', 'hamerly'};
best = zeros(1, numel(engines));
idxRef = [];
CRef = [];
sumdRef = [];
for e = 1:numel(engines)
  t = inf;
  for k = 1:runs
    tic();
    [idx, C, sumd] = kmeans(X, m, 'Start', start, 'MaxIter', 30, ...
      'OnlinePhase', 'off', 'Options', opts, 'Engine', engines{e});
    t = min(t, toc());
  end
  best(e) = t;
  if e == 1
    idxRef = idx; CRef = C; sumdRef = sumd;
  else
    asserts.isequal(idx, idxRef);
    asserts.isequal(C, CRef);
    asserts.isequal(sumd, sumdRef);
  end
end
%=============================================================================
disp(['kmeans engines observations: ', int2str(size(X, 1)), ', clusters: ', int2str(m), ...
  ', dims: ', int2str(dims)]);
for e = 1:numel(engines)
  disp(['  ', engines{e}, ' best time: ', num2str(best(e)), ' s  (speedup vs legacy ', ...
    num2str(best(1) / best(e)), 'x)']);
end
%=============================================================================
