%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = tuneWeights_resultCases(results)
  if ~tuneWeights_isResult(results) || ~isfield(results, 'cases') %#ok<NLS0004>
    error('Nelson:tests_manager:testRunResultExpected', 'TestRunResult expected.');
  end
  normalized = results.cases;
  raw = run_resultCases(results); %#ok<NLS0004>
  planCases = struct([]);
  if isfield(results, 'plan') && isfield(results.plan, 'cases')
    planCases = results.plan.cases;
  end
  cases = struct([]);
  for k = 1:length(normalized)
    item = tuneWeights_measuredCase(normalized(k), raw, planCases, k);
    if isempty(item)
      continue
    end
    cases = tuneWeights_mergeMeasuredCase(cases, item);
  end
end
%=============================================================================
function item = tuneWeights_measuredCase(testCase, raw, planCases, index)
  item = struct([]);
  if ~tuneWeights_isMeasuredOutcome(testCase)
    return
  end
  candidate = tuneWeights_caseMetadata(testCase, raw, planCases, index);
  if isempty(candidate.file) || ~isfile(candidate.file)
    return
  end
  item = candidate;
end
%=============================================================================
function cases = tuneWeights_mergeMeasuredCase(cases, item)
  duplicate = tuneWeights_findFile(cases, item.file);
  if duplicate == 0
    cases = [cases; item];
  elseif item.duration > cases(duplicate).duration
    cases(duplicate) = item;
  end
end
%=============================================================================
function tf = tuneWeights_isMeasuredOutcome(testCase)
  tf = isfield(testCase, 'outcome') ...
    && (strcmp(testCase.outcome, 'passed') || strcmp(testCase.outcome, 'bench')) ...
    && isfield(testCase, 'duration') && isnumeric(testCase.duration) ...
    && isscalar(testCase.duration) && isfinite(testCase.duration) ...
    && testCase.duration >= 0;
end
%=============================================================================
function item = tuneWeights_caseMetadata(testCase, raw, planCases, index)
  rawCase = struct();
  planCase = struct();
  if index <= length(raw)
    rawCase = raw(index);
  end
  if index <= length(planCases)
    planCase = planCases(index);
  end
  file = tuneWeights_firstField(testCase, rawCase, planCase, {'file', 'filename'}, '');
  name = tuneWeights_firstField(testCase, rawCase, planCase, {'name', 'id'}, '');
  if isempty(name) && ~isempty(file)
    [~, name, ~] = fileparts(file); %#ok<NLS0004>
  end
  moduleName = tuneWeights_firstField(testCase, rawCase, planCase, {'module'}, '');
  if isempty(moduleName)
    moduleName = discover_moduleNameFromFile(file); %#ok<NLS0004>
  end
  kind = tuneWeights_firstField(testCase, rawCase, planCase, {'kind'}, '');
  if isempty(kind)
    kind = discover_caseKind(name); %#ok<NLS0004>
  end
  mode = tuneWeights_firstField(testCase, rawCase, planCase, {'mode'}, 'cli');
  item = struct('file', file, 'name', name, 'module', moduleName, 'kind', kind, ...
    'mode', mode, 'duration', double(testCase.duration));
end
%=============================================================================
function value = tuneWeights_firstField(first, second, third, names, defaultValue)
  value = tuneWeights_structField(first, names);
  if isempty(value)
    value = tuneWeights_structField(second, names);
  end
  if isempty(value)
    value = tuneWeights_structField(third, names);
  end
  if isempty(value)
    value = defaultValue;
  end
end
%=============================================================================
function value = tuneWeights_structField(source, names)
  value = [];
  for n = 1:length(names)
    if isfield(source, names{n}) && ~isempty(source.(names{n}))
      value = source.(names{n});
      return
    end
  end
end
%=============================================================================
function index = tuneWeights_findFile(cases, filename)
  index = 0;
  for k = 1:length(cases)
    if strcmp(cases(k).file, filename)
      index = k;
      return
    end
  end
end
%=============================================================================
