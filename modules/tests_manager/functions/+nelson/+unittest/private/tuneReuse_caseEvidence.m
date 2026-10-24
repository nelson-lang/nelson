%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function evidence = tuneReuse_caseEvidence(results, filename, kind, expected, requireReuse)
  selected = tuneReuse_resultCases(results, filename);
  durations = [];
  ok = length(selected) == expected;
  for k = 1:length(selected)
    testCase = selected(k);
    ok = ok && tuneReuse_successfulOutcome(testCase.outcome, kind);
    if requireReuse
      ok = ok && tuneReuse_cleanWorkerResult(testCase);
    end
    if testCase.duration >= 0
      durations = [durations, testCase.duration];
    end
  end
  duration = 0;
  if ~isempty(durations)
    duration = median(durations);
  end
  evidence = struct('ok', logical(ok), 'runs', length(selected), ...
    'duration', duration);
end
%=============================================================================
function selected = tuneReuse_resultCases(results, filename)
  selected = struct([]);
  if ~isstruct(results) || ~isfield(results, 'cases')
    return
  end
  for k = 1:length(results.cases)
    if strcmp(results.cases(k).file, filename)
      selected = [selected; results.cases(k)];
    end
  end
end
%=============================================================================
function tf = tuneReuse_successfulOutcome(outcome, kind)
  if strcmp(kind, 'bench')
    tf = strcmp(outcome, 'bench');
  else
    tf = strcmp(outcome, 'passed');
  end
end
%=============================================================================
function tf = tuneReuse_cleanWorkerResult(testCase)
  tf = false;
  if ~isfield(testCase, 'diagnostics') ...
    || ~isfield(testCase.diagnostics, 'worker_pool')
    return
  end
  worker = testCase.diagnostics.worker_pool;
  required = {'used', 'fallback', 'missing_result'};
  if ~all(isfield(worker, required))
    return
  end
  tf = worker.used && ~worker.fallback && ~worker.missing_result;
end
%=============================================================================
