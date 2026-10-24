%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nc_perf_entry(kind, countText, sampleText, threadText)
  if strcmp(kind, 'startup')
    fprintf('NELSONC_STARTUP_OK\n');
    return;
  end
  count = str2double(countText);
  samples = str2double(sampleText);
  if nargin() < 4
    threadText = '1';
  end
  threads = str2double(threadText);
  if isnan(threads) || isinf(threads) || threads < 1 || threads > 64 || threads ~= fix(threads)
    error('Nelson:nelsonc:benchmarkThreads', 'Invalid benchmark thread count.');
  end
  previousThreads = maxNumCompThreads(threads);
  restoreThreads = onCleanup(@() maxNumCompThreads(previousThreads));
  expected = nc_perf_work(kind, count);
  wall = zeros(1, samples);
  cpu = zeros(1, samples);
  for k = 1:samples
    cpuStart = cputime();
    tic();
    result = nc_perf_work(kind, count);
    wall(k) = toc();
    cpu(k) = cputime() - cpuStart;
    if result ~= expected
      error('Benchmark result changed between repetitions.');
    end
  end
  record = struct('kind', kind, 'count', count, 'wall', wall, 'cpu', cpu, ...
    'result', result, 'threads', maxNumCompThreads());
  fprintf('NELSONC_PERF=%s\n', jsonencode(record));
  clear restoreThreads;
end
