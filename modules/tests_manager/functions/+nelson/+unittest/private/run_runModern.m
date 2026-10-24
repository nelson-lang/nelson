%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function results = run_runModern(targetsOrPlan, varargin)
  runBegin = time();
  cleanupOnExit = ~is_unittest_runfile_worker();
  try
    options = run_parseRunOptions(varargin);
    cleanupLauncher = run_applyLauncherBackend(options.Launcher); %#ok<NLS0004>
    cleanupExecutionMode = run_applyExecutionMode(options.ExecutionMode); %#ok<NLS0004>
    plan = run_ensurePlan(targetsOrPlan, options);
    if strcmpi(options.ExecutionMode, 'inline')
      plan.workers = 1;
      plan.resource_policy = 'inline';
    end
    %#ok<NLS0004>
    [cleanupRegistry, registry] = run_registerRun(plan, '');
    requestedWorkers = plan.workers;
    if requestedWorkers < 1
      requestedWorkers = 1;
    end
    previousWorkers = maxNumCompThreads(requestedWorkers);
    %#ok<NLS0004>
    cleanupWorkers = onCleanup(@() maxNumCompThreads(previousWorkers));
    [status, raw] = run_runPlan(plan, options); %#ok<NLS0004>
    raw = run_annotateAttempts(raw, 1);
    [raw, status] = run_retryFailedCases(raw, plan, options, status); %#ok<NLS0004>
    if cleanupOnExit
      run_cleanupProcesses(false);
    end
    clear cleanupWorkers
    clear cleanupRegistry
    raw.total_time = time() - runBegin;
    results = run_normalizeResults(raw, plan, status);
    results.registry = registry;
    results.options = struct('Verbose', logical(options.Verbose), ...
      'ExecutionMode', lower(options.ExecutionMode));
    results = run_writeLogs(results, options.LogDir);
    run_writeRequestedReport(results, options);
    raw.total_time = time() - runBegin;
    results.raw = raw;
    results.summary.duration = raw.total_time;
    test_run_disp_summary(raw);
  catch ex
    if cleanupOnExit
      run_cleanupProcesses(true);
    end
    rethrow(ex);
  end
end
