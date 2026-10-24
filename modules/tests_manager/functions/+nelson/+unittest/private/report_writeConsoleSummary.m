%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function report_writeConsoleSummary(results, verbose)
  if nargin < 2
    verbose = false;
  end
  summary = results.summary;
  tests = report_summaryValue(summary, 'tests');
  if tests == 0 && isfield(results, 'cases')
    tests = length(results.cases);
  end
  line = ['nelson.unittest: ', int2str(tests), ' tests, ', ...
    int2str(report_summaryValue(summary, 'passed')), ' passed, ', ...
    int2str(report_summaryValue(summary, 'failed')), ' failed, ', ...
    int2str(report_summaryValue(summary, 'skipped')), ' skipped, ', ...
    int2str(report_summaryValue(summary, 'filtered')), ' filtered, ', ...
    int2str(report_summaryValue(summary, 'timeout')), ' timeout, ', ...
    int2str(report_summaryValue(summary, 'error')), ' error, ', ...
    int2str(report_summaryValue(summary, 'bench')), ' bench, ', ...
    report_durationText(report_summaryValue(summary, 'duration')), 's'];
  fprintf('%s\n', line);
  if verbose && isfield(summary, 'worker_pool')
    report_writeWorkerPoolSummary(summary.worker_pool);
  end
end
