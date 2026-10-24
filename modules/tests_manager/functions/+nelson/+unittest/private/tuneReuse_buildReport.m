%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = tuneReuse_buildReport(calibration, options)
  reportCases = struct([]);
  for k = 1:length(calibration.cases)
    planCase = calibration.cases(k);
    baseline = tuneReuse_caseEvidence(...
      calibration.baseline, planCase.file, planCase.kind, 2, false); %#ok<NLS0004>
    reused = tuneReuse_reusedEvidence(calibration, planCase);
    timing = tuneReuse_moduleTiming(calibration, planCase.module);
    hadTag = tuneReuse_reportHasTag(planCase);
    [action, reason] = tuneReuse_action(... %#ok<NLS0024>
      hadTag, baseline.ok, reused.ok, timing.gain, options);
    item = struct('file', planCase.file, 'name', planCase.name, ...
      'module', planCase.module, 'kind', planCase.kind, 'mode', planCase.mode, ...
      'had_tag', logical(hadTag), 'baseline_ok', baseline.ok, ...
      'reuse_ok', reused.ok, 'baseline_runs', baseline.runs, ...
      'reuse_runs', reused.runs, 'baseline_test_duration', baseline.duration, ...
      'reuse_test_duration', reused.duration, ...
      'isolated_duration', timing.isolated, ...
      'reuse_duration', timing.reused, 'gain', timing.gain, ...
      'action', action, 'reason', reason, 'applied', false);
    reportCases = [reportCases; item];
  end
  if options.Apply
    reportCases = tuneReuse_apply(reportCases);
  end
  report = struct();
  report.type = 'ReuseTuningResult';
  report.applied = logical(options.Apply);
  report.allow_add = logical(options.AllowAdd);
  report.trials = options.Trials;
  report.min_gain = options.MinGain;
  report.strategy = 'isolated-vs-reused-two-pass-deterministic';
  report.duration = calibration.duration;
  report.cases = reportCases;
  report.summary = tuneReuse_summary(reportCases, calibration);
end
%=============================================================================
function evidence = tuneReuse_reusedEvidence(calibration, planCase)
  ok = true;
  runs = 0;
  durations = [];
  for k = 1:length(calibration.reuse_trials)
    current = tuneReuse_caseEvidence(...
      calibration.reuse_trials{k}, planCase.file, planCase.kind, 2, true); %#ok<NLS0004>
    ok = ok && current.ok;
    runs = runs + current.runs;
    durations = [durations, current.duration];
  end
  duration = 0;
  if ~isempty(durations)
    duration = median(durations);
  end
  evidence = struct('ok', logical(ok), 'runs', runs, 'duration', duration);
end
%=============================================================================
function timing = tuneReuse_moduleTiming(calibration, moduleName)
  isolated = tuneReuse_suiteDuration(calibration.baseline, moduleName);
  durations = zeros(1, length(calibration.reuse_trials));
  for k = 1:length(calibration.reuse_trials)
    durations(k) = tuneReuse_suiteDuration(calibration.reuse_trials{k}, moduleName);
  end
  reused = 0;
  if ~isempty(durations)
    reused = median(durations);
  end
  gain = 0;
  if isolated > 0
    gain = (isolated - reused) / isolated;
  end
  timing = struct('isolated', isolated, 'reused', reused, 'gain', gain);
end
%=============================================================================
function duration = tuneReuse_suiteDuration(results, moduleName)
  duration = 0;
  if isempty(results) || ~isfield(results, 'raw') ...
    || ~isfield(results.raw, 'tests_list')
    return
  end
  suiteName = run_planSuiteName(moduleName);
  for k = 1:length(results.raw.tests_list)
    suite = results.raw.tests_list(k);
    if strcmp(suite.name, suiteName)
      duration = suite.time;
      return
    end
  end
end
%=============================================================================
function tf = tuneReuse_reportHasTag(planCase)
  tf = any(strcmp(planCase.tags, 'reuse_process')) ...
    || any(strcmp(planCase.resources, 'reuse_process'));
end
%=============================================================================
function [action, reason] = tuneReuse_action(hadTag, baselineOk, reuseOk, gain, options)
  action = 'unchanged';
  if ~baselineOk
    reason = 'baseline_failed';
  elseif ~reuseOk
    reason = 'reuse_failed';
    if hadTag
      action = 'remove';
    end
  elseif hadTag
    reason = 'certified';
  elseif gain >= options.MinGain
    action = 'add';
    reason = tuneReuse_addReason(gain);
  else
    reason = 'insufficient_gain';
  end
end
%=============================================================================
function reason = tuneReuse_addReason(gain)
  reason = 'safe_and_beneficial';
  if gain < 0
    reason = 'safety_only';
  end
end
%=============================================================================
function cases = tuneReuse_apply(cases)
  for k = 1:length(cases)
    if strcmp(cases(k).action, 'add')
      tuneReuse_rewriteFile(cases(k).file, true); %#ok<NLS0004>
      cases(k).applied = true;
    elseif strcmp(cases(k).action, 'remove')
      tuneReuse_rewriteFile(cases(k).file, false); %#ok<NLS0004>
      cases(k).applied = true;
    end
  end
end
%=============================================================================
function summary = tuneReuse_summary(cases, calibration)
  summary = struct('selected', calibration.selected, ...
    'eligible', calibration.eligible, 'calibrated', length(cases), ...
    'excluded', calibration.excluded, 'deferred', calibration.deferred, ...
    'added', 0, 'removed', 0, 'unchanged', 0, 'written', 0, ...
    'baseline_failed', 0, 'reuse_failed', 0, 'certified', 0);
  for k = 1:length(cases)
    summary = tuneReuse_countAction(summary, cases(k));
  end
end
%=============================================================================
function summary = tuneReuse_countAction(summary, testCase)
  if strcmp(testCase.action, 'add')
    summary.added = summary.added + 1;
  elseif strcmp(testCase.action, 'remove')
    summary.removed = summary.removed + 1;
  else
    summary.unchanged = summary.unchanged + 1;
  end
  if testCase.applied
    summary.written = summary.written + 1;
  end
  if isfield(summary, testCase.reason)
    summary.(testCase.reason) = summary.(testCase.reason) + 1;
  end
end
%=============================================================================
