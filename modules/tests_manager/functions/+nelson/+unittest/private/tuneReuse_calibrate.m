%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function calibration = tuneReuse_calibrate(targets, runArguments, options)
  runOptions = run_parseRunOptions(runArguments);
  runOptions.Workers = 1;
  runOptions.StopOnFail = false;
  runOptions.Retry = 0;
  runOptions.Shuffle = false;
  runOptions.ShardIndex = 1;
  runOptions.ShardCount = 1;
  sourcePlan = run_ensurePlan(targets, runOptions);
  sourceCases = tuneReuse_uniqueCases(sourcePlan.cases);
  [cases, counts] = tuneReuse_selectCases(sourceCases, options.AllowAdd); %#ok<NLS0024>
  calibration = tuneReuse_emptyCalibration(cases, counts, options);
  if isempty(cases)
    return
  end
  previousForceNative = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
  cleanupForceNative = onCleanup(... %#ok<NLS0004>
    @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNative));
  setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
  disp([newline, _('Reuse calibration: isolated baseline.')]);
  baselinePlan = tuneReuse_campaignPlan(sourcePlan, cases, false, 1);
  calibration.baseline = tuneReuse_runCampaign(baselinePlan, runOptions.Timeout);
  for trial = 1:options.Trials
    fprintf('\n%s %d/%d.\n', _('Reuse calibration: trial'), trial, options.Trials);
    trialPlan = tuneReuse_campaignPlan(sourcePlan, cases, true, trial);
    calibration.reuse_trials{trial} = tuneReuse_runCampaign(trialPlan, runOptions.Timeout);
  end
  calibration.duration = tuneReuse_calibrationDuration(calibration);
  clear cleanupForceNative
end
%=============================================================================
function cases = tuneReuse_uniqueCases(sourceCases)
  cases = struct([]);
  filenames = {};
  for k = 1:length(sourceCases)
    filename = sourceCases(k).file;
    if ~any(strcmp(filenames, filename))
      cases = [cases; sourceCases(k)];
      filenames = [filenames; {filename}];
    end
  end
end
%=============================================================================
function [selected, counts] = tuneReuse_selectCases(cases, allowAdd)
  selected = struct([]);
  counts = struct('selected', length(cases), 'eligible', 0, 'excluded', 0, 'deferred', 0);
  for k = 1:length(cases)
    eligible = tuneReuse_isEligible(cases(k));
    tagged = tuneReuse_hasTag(cases(k));
    if ~eligible
      counts.excluded = counts.excluded + 1;
    else
      counts.eligible = counts.eligible + 1;
      if tagged || allowAdd
        selected = [selected; cases(k)];
      else
        counts.deferred = counts.deferred + 1;
      end
    end
  end
end
%=============================================================================
function tf = tuneReuse_isEligible(planCase)
  tagOptions = run_tagOptionsFromPlanCase(planCase);
  tagOptions.reuse_process = true;
  tf = can_pool_test_case_mode(tagOptions) ...
    && can_pool_test_case_resources(tagOptions);
end
%=============================================================================
function tf = tuneReuse_hasTag(planCase)
  tf = tuneReuse_hasValue(planCase, 'tags') ...
    || tuneReuse_hasValue(planCase, 'resources');
end
%=============================================================================
function tf = tuneReuse_hasValue(planCase, field)
  tf = isfield(planCase, field) ...
    && any(strcmp(planCase.(field), 'reuse_process'));
end
%=============================================================================
function calibration = tuneReuse_emptyCalibration(cases, counts, options)
  calibration = struct();
  calibration.type = 'ReuseCalibration';
  calibration.cases = cases;
  calibration.selected = counts.selected;
  calibration.eligible = counts.eligible;
  calibration.excluded = counts.excluded;
  calibration.deferred = counts.deferred;
  calibration.trials = options.Trials;
  calibration.baseline = [];
  calibration.reuse_trials = cell(1, options.Trials);
  calibration.duration = struct('isolated', 0, 'reuse_trials', [], ...
    'reuse_median', 0, 'gain', 0);
end
%=============================================================================
function plan = tuneReuse_campaignPlan(sourcePlan, cases, reuseProcess, trial)
  ordered = tuneReuse_orderCases(cases, trial);
  campaignCases = [ordered; ordered];
  for k = 1:length(campaignCases)
    campaignCases(k) = tuneReuse_setTag(campaignCases(k), reuseProcess);
    campaignCases(k).id = createGUID();
  end
  plan = sourcePlan;
  plan.cases = campaignCases;
  plan.workers = 1;
  plan.shard = [1, 1];
  plan.groups = plan_groupCases(campaignCases);
  plan.filtered = 0;
end
%=============================================================================
function ordered = tuneReuse_orderCases(cases, trial)
  nbCases = length(cases);
  indices = 1:nbCases;
  if trial == 2
    indices = nbCases:-1:1;
  elseif trial > 2 && nbCases > 1
    shift = mod(trial - 2, nbCases);
    indices = [shift + 1:nbCases, 1:shift];
  end
  ordered = cases(indices);
end
%=============================================================================
function planCase = tuneReuse_setTag(planCase, enabled)
  planCase.tags = tuneReuse_withoutTag(planCase.tags);
  planCase.resources = tuneReuse_withoutTag(planCase.resources);
  if enabled
    planCase.tags = [planCase.tags(:); {'reuse_process'}];
  end
end
%=============================================================================
function values = tuneReuse_withoutTag(values)
  values = values(:);
  values(strcmp(values, 'reuse_process')) = [];
end
%=============================================================================
function results = tuneReuse_runCampaign(plan, timeoutValue)
  runArgs = {'StopOnFail', false, 'Retry', 0, 'Timeout', timeoutValue};
  results = nelson.unittest.run(plan, runArgs{:});
end
%=============================================================================
function duration = tuneReuse_calibrationDuration(calibration)
  isolated = calibration.baseline.summary.duration;
  reuseDurations = zeros(1, length(calibration.reuse_trials));
  for k = 1:length(calibration.reuse_trials)
    reuseDurations(k) = calibration.reuse_trials{k}.summary.duration;
  end
  reuseMedian = median(reuseDurations);
  gain = tuneReuse_gain(isolated, reuseMedian);
  duration = struct('isolated', isolated, 'reuse_trials', reuseDurations, ...
    'reuse_median', reuseMedian, 'gain', gain);
end
%=============================================================================
function gain = tuneReuse_gain(isolated, reused)
  gain = 0;
  if isolated > 0
    gain = (isolated - reused) / isolated;
  end
end
%=============================================================================
