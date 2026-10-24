%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function report = tuneWeights_buildReport(results, options)
  measuredCases = tuneWeights_resultCases(results); %#ok<NLS0004>
  reportCases = struct([]);
  for k = 1:length(measuredCases)
    testCase = measuredCases(k);
    reference = tuneWeights_referenceDuration(measuredCases, testCase);
    proposed = tuneWeights_quantize(testCase.duration, reference, options.MaxWeight);
    state = tuneWeights_fileState(testCase.file); %#ok<NLS0004>
    action = tuneWeights_action(state.has_tag, state.weight, proposed);
    item = struct('file', testCase.file, 'name', testCase.name, ...
      'module', testCase.module, 'kind', testCase.kind, 'mode', testCase.mode, ...
      'duration', testCase.duration, 'reference_duration', reference, ...
      'old_weight', state.weight, 'new_weight', proposed, ...
      'action', action, 'applied', false);
    reportCases = [reportCases; item];
  end
  if options.Apply
    for k = 1:length(reportCases)
      if ~strcmp(reportCases(k).action, 'unchanged')
        tuneWeights_rewriteFile(reportCases(k).file, reportCases(k).new_weight); %#ok<NLS0004>
        reportCases(k).applied = true;
      end
    end
  end
  report = struct();
  report.type = 'WeightTuningResult';
  report.applied = logical(options.Apply);
  report.max_weight = options.MaxWeight;
  report.strategy = 'median-relative-powers-of-two';
  report.cases = reportCases;
  report.summary = tuneWeights_reportSummary(reportCases);
end
%=============================================================================
function reference = tuneWeights_referenceDuration(cases, selected)
  durations = [];
  selectedBench = strcmp(selected.kind, 'bench');
  for k = 1:length(cases)
    sameCategory = strcmp(cases(k).kind, 'bench') == selectedBench;
    if strcmp(cases(k).module, selected.module) && sameCategory && cases(k).duration > 0
      durations = [durations, cases(k).duration];
    end
  end
  if isempty(durations)
    reference = 1;
  else
    reference = max(0.001, median(durations));
  end
end
%=============================================================================
function weight = tuneWeights_quantize(duration, reference, maxWeight)
  weight = 1;
  ratio = duration / reference;
  threshold = sqrt(2);
  while weight < maxWeight && ratio > weight * threshold
    weight = min(maxWeight, weight * 2);
  end
end
%=============================================================================
function action = tuneWeights_action(hasTag, oldWeight, newWeight)
  if ~hasTag && newWeight == 1
    action = 'unchanged';
  elseif ~hasTag
    action = 'add';
  elseif newWeight == 1
    action = 'remove';
  elseif oldWeight ~= newWeight
    action = 'update';
  else
    action = 'unchanged';
  end
end
%=============================================================================
function summary = tuneWeights_reportSummary(cases)
  summary = struct('eligible', length(cases), 'added', 0, 'updated', 0, ...
    'removed', 0, 'unchanged', 0, 'written', 0);
  for k = 1:length(cases)
    field = tuneWeights_summaryField(cases(k).action);
    summary.(field) = summary.(field) + 1;
    if cases(k).applied
      summary.written = summary.written + 1;
    end
  end
end
%=============================================================================
function field = tuneWeights_summaryField(action)
  field = [action, 'd'];
  if strcmp(action, 'add')
    field = 'added';
  elseif strcmp(action, 'remove')
    field = 'removed';
  elseif strcmp(action, 'unchanged')
    field = 'unchanged';
  end
end
%=============================================================================
