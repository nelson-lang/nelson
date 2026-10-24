%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, history] = sequentialfs(fun, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 2);
  if ~isfunction_handle(fun)
    error('Nelson:statistics:FUNFunctionHandle', '%s must be a function handle.', 'FUN');
  end
  [data, opts] = parseInputsLocal(varargin);
  validateDataLocal(data);
  nfeatures = size(data{1}, 2);
  opts = finalizeOptionsLocal(opts, nfeatures);
  [tf, history] = selectFeaturesLocal(fun, data, opts);
end
%=============================================================================
function [data, opts] = parseInputsLocal(args)
  names = {'cv', 'mcreps', 'direction', 'keepin', 'keepout', ...
    'nfeatures', 'nullmodel', 'options'};
  firstName = length(args) + 1;
  for k = 1:length(args)
    if isTextScalarLocal(args{k}) && any(strcmpi(char(args{k}), names))
      firstName = k;
      break
    end
  end
  data = args(1:firstName - 1);
  if isempty(data)
    error('Nelson:statistics:leastOneDataArgumentRequired', 'At least one %s is required.', 'data input');
  end
  opts = defaultOptionsLocal();
  nv = args(firstName:end);
  if rem(length(nv), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(nv)
    name = lower(char(nv{k}));
    value = nv{k + 1};
    switch name
      case 'cv'
        opts.CV = value;
      case 'mcreps'
        opts.MCReps = value;
      case 'direction'
        opts.Direction = parseDirectionLocal(value);
      case 'keepin'
        opts.KeepIn = value;
      case 'keepout'
        opts.KeepOut = value;
      case 'nfeatures'
        opts.NFeatures = value;
      case 'nullmodel'
        opts.NullModel = parseLogicalScalarLocal(value, 'NullModel');
      case 'options'
        opts.Options = value;
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
  end
end
%=============================================================================
function opts = defaultOptionsLocal()
  opts = struct('CV', 10, 'MCReps', 1, 'Direction', 'forward', ...
    'KeepIn', [], 'KeepOut', [], 'NFeatures', [], ...
    'NullModel', false, 'Options', []);
end
%=============================================================================
function validateDataLocal(data)
  nobs = size(data{1}, 1);
  if ~(isnumeric(data{1}) && isreal(data{1}) && ~issparse(data{1}) && ndims(data{1}) == 2)
    error('Nelson:statistics:firstDataInputRealNumeric', '%s must be a real numeric matrix.', 'First data input');
  end
  if isempty(data{1})
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'First data input');
  end
  for k = 1:length(data)
    if size(data{k}, 1) ~= nobs
      error('Nelson:statistics:allDataInputsSameNumber', '%s must have the same number of rows.', 'All data inputs');
    end
  end
end
%=============================================================================
function opts = finalizeOptionsLocal(opts, nfeatures)
  opts.KeepIn = parseFeatureMaskLocal(opts.KeepIn, nfeatures, false, 'KeepIn');
  opts.KeepOut = parseFeatureMaskLocal(opts.KeepOut, nfeatures, false, 'KeepOut');
  if any(opts.KeepIn & opts.KeepOut)
    error('Nelson:statistics:keepInKeepOutOverlap', 'KeepIn and KeepOut must not overlap.');
  end
  opts.NFeatures = parseNFeaturesLocal(opts.NFeatures, nfeatures);
  opts.MCReps = parsePositiveIntegerLocal(opts.MCReps, 'MCReps');
  opts.CV = parseCVLocal(opts.CV);
  opts.Options = parseOptionsLocal(opts.Options, nfeatures);
end
%=============================================================================
function value = parseDirectionLocal(value)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Direction');
  end
  value = lower(char(value));
  choices = {'forward', 'backward'};
  matches = choices(strncmp(choices, value, length(value)));
  if isempty(matches)
    error('Nelson:statistics:invalidDirectionValue', 'Invalid %s value.', 'Direction');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousValue', 'Ambiguous %s value.', 'Direction');
  end
  value = matches{1};
end
%=============================================================================
function mask = parseFeatureMaskLocal(value, nfeatures, defaultValue, name)
  mask = repmat(defaultValue, 1, nfeatures);
  if isempty(value)
    return
  end
  if islogical(value) && isvector(value)
    if length(value) ~= nfeatures
      error('Nelson:statistics:mustMatchFeatures', '%s must match the number of features.', name);
    end
    mask = value(:)' ~= 0;
  elseif isnumeric(value) && isreal(value) && isvector(value)
    idx = double(value(:))';
    if any(~isfinite(idx)) || any(floor(idx) ~= idx) || any(idx < 1) || any(idx > nfeatures)
      error('Nelson:statistics:invalidFeatureIndices', '%s contains invalid feature indices.', name);
    end
    mask(:) = false;
    mask(idx) = true;
  else
    error('Nelson:statistics:invalidValue:valueHasInvalidValue', '%s has an invalid value.', name);
  end
end
%=============================================================================
function value = parseNFeaturesLocal(value, nfeatures)
  if isempty(value)
    return
  end
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && floor(value) == value && value >= 0 && value <= nfeatures)
    error('Nelson:statistics:nFeaturesIntegerScalarBetween0', 'NFeatures must be an integer scalar between 0 and the number of features.');
  end
  value = double(value);
end
%=============================================================================
function value = parseCVLocal(value)
  if isTextScalarLocal(value)
    value = lower(char(value));
    if ~any(strcmp(value, {'none', 'resubstitution'}))
      error('Nelson:statistics:CVInvalidValue', '%s has an invalid value.', 'CV');
    end
  elseif isnumeric(value) && isreal(value) && isscalar(value)
    value = parsePositiveIntegerLocal(value, 'CV');
  else
    error('Nelson:statistics:CVPositiveIntegerNoneResubstitution', 'CV must be a positive integer, none, or resubstitution.');
  end
end
%=============================================================================
function options = parseOptionsLocal(options, nfeatures)
  if isempty(options)
    options = statset();
  elseif ~(isstruct(options) && isscalar(options))
    error('Nelson:statistics:invalidOptions', '%s must be a scalar structure.', 'Options');
  end
  displayValue = statget(options, 'Display', 'off');
  if ~isTextScalarLocal(displayValue)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Options Display');
  end
  options.Display = lower(char(displayValue));
  options.MaxIter = statget(options, 'MaxIter', nfeatures);
  options.TolFun = statget(options, 'TolFun', 1e-6);
  if ~any(strcmp(options.Display, {'off', 'final', 'iter'}))
    error('Nelson:statistics:optionsDisplayInvalidValue', '%s has an invalid value.', 'Options Display');
  end
  if ~isPositiveIntegerLocal(options.MaxIter)
    error('Nelson:statistics:optionsMaxIterPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Options MaxIter');
  end
  if ~(isnumeric(options.TolFun) && isreal(options.TolFun) && isscalar(options.TolFun) ...
      && isfinite(options.TolFun) && options.TolFun >= 0)
    error('Nelson:statistics:optionsTolFunNonnegativeScalar', '%s must be a nonnegative scalar.', 'Options TolFun');
  end
end
%=============================================================================
function [tf, history] = selectFeaturesLocal(fun, data, opts)
  if strcmp(opts.Direction, 'backward')
    [tf, history] = backwardLocal(fun, data, opts);
  else
    [tf, history] = forwardLocal(fun, data, opts);
  end
end
%=============================================================================
function [tf, history] = forwardLocal(fun, data, opts)
  tf = opts.KeepIn;
  available = ~(opts.KeepOut | tf);
  currentCrit = initialCriterionLocal(fun, data, tf, opts, Inf);
  history = appendHistoryLocal([], tf, currentCrit);
  for iter = 1:opts.Options.MaxIter
    if reachedTargetLocal(tf, opts, 'forward')
      break
    end
    [candidate, bestCrit] = bestForwardCandidateLocal(fun, data, tf, available, opts);
    if candidate == 0 || (~isinf(currentCrit) && currentCrit - bestCrit <= opts.Options.TolFun)
      break
    end
    tf(candidate) = true;
    available(candidate) = false;
    currentCrit = bestCrit;
    history = appendHistoryLocal(history, tf, currentCrit);
    displayStepLocal(opts, iter, 'added', candidate, currentCrit);
  end
end
%=============================================================================
function [tf, history] = backwardLocal(fun, data, opts)
  tf = ~opts.KeepOut;
  currentCrit = criterionLocal(fun, data, tf, opts);
  history = appendHistoryLocal([], tf, currentCrit);
  for iter = 1:opts.Options.MaxIter
    if reachedTargetLocal(tf, opts, 'backward')
      break
    end
    removable = find(tf & ~opts.KeepIn);
    [candidate, bestCrit] = bestBackwardCandidateLocal(fun, data, tf, removable, opts);
    if candidate == 0 || currentCrit - bestCrit <= opts.Options.TolFun
      break
    end
    tf(candidate) = false;
    currentCrit = bestCrit;
    history = appendHistoryLocal(history, tf, currentCrit);
    displayStepLocal(opts, iter, 'removed', candidate, currentCrit);
  end
end
%=============================================================================
function crit = initialCriterionLocal(fun, data, tf, opts, defaultValue)
  if opts.NullModel || any(tf)
    crit = criterionLocal(fun, data, tf, opts);
  else
    crit = defaultValue;
  end
end
%=============================================================================
function [candidate, bestCrit] = bestForwardCandidateLocal(fun, data, tf, available, opts)
  candidate = 0;
  bestCrit = Inf;
  candidates = find(available);
  for i = 1:length(candidates)
    trial = tf;
    trial(candidates(i)) = true;
    crit = criterionLocal(fun, data, trial, opts);
    if crit < bestCrit
      bestCrit = crit;
      candidate = candidates(i);
    end
  end
end
%=============================================================================
function [candidate, bestCrit] = bestBackwardCandidateLocal(fun, data, tf, removable, opts)
  candidate = 0;
  bestCrit = Inf;
  for i = 1:length(removable)
    trial = tf;
    trial(removable(i)) = false;
    if ~opts.NullModel && ~any(trial)
      continue
    end
    crit = criterionLocal(fun, data, trial, opts);
    if crit < bestCrit
      bestCrit = crit;
      candidate = removable(i);
    end
  end
end
%=============================================================================
function tf = reachedTargetLocal(selected, opts, direction)
  tf = false;
  if isempty(opts.NFeatures)
    return
  end
  if strcmp(direction, 'forward')
    tf = sum(selected) >= opts.NFeatures;
  else
    tf = sum(selected) <= opts.NFeatures;
  end
end
%=============================================================================
function crit = criterionLocal(fun, data, selected, opts)
  if ischar(opts.CV) && strcmp(opts.CV, 'none')
    args = subsetDataLocal(data, selected, []);
    crit = feval(fun, args{:});
  elseif ischar(opts.CV) && strcmp(opts.CV, 'resubstitution')
    args = [subsetDataLocal(data, selected, []), subsetDataLocal(data, selected, [])];
    crit = feval(fun, args{:});
  else
    crit = kfoldCriterionLocal(fun, data, selected, opts.CV);
  end
  if ~(isnumeric(crit) && isreal(crit) && isscalar(crit) && isfinite(crit))
    error('Nelson:statistics:criterionFunctionReturnFiniteReal', 'Criterion function must return a finite real scalar.');
  end
  crit = double(crit);
end
%=============================================================================
function crit = kfoldCriterionLocal(fun, data, selected, kfold)
  nobs = size(data{1}, 1);
  kfold = min(kfold, nobs);
  fold = rem(0:nobs - 1, kfold) + 1;
  total = 0;
  totalTest = 0;
  for k = 1:kfold
    test = fold(:) == k;
    train = ~test;
    args = [subsetDataLocal(data, selected, train), subsetDataLocal(data, selected, test)];
    value = feval(fun, args{:});
    total = total + double(value);
    totalTest = totalTest + sum(test);
  end
  crit = total / max(totalTest, 1);
end
%=============================================================================
function out = subsetDataLocal(data, selected, rows)
  out = cell(1, length(data));
  for k = 1:length(data)
    value = data{k};
    if isempty(rows)
      rowIdx = 1:size(value, 1);
    else
      rowIdx = rows;
    end
    if k == 1
      out{k} = value(rowIdx, selected);
    else
      out{k} = value(rowIdx, :);
    end
  end
end
%=============================================================================
function history = appendHistoryLocal(history, tf, crit)
  if isempty(history)
    history = struct('In', logical(tf), 'Crit', crit);
  else
    history.In(end + 1, :) = logical(tf);
    history.Crit(end + 1) = crit;
  end
end
%=============================================================================
function displayStepLocal(opts, iter, action, column, crit)
  if strcmp(opts.Options.Display, 'iter')
    disp(['Step ', int2str(iter), ', ', action, ' column ', ...
      int2str(column), ', criterion value ', num2str(crit)]);
  elseif strcmp(opts.Options.Display, 'final')
    return
  end
end
%=============================================================================
function value = parsePositiveIntegerLocal(value, name)
  if ~isPositiveIntegerLocal(value)
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function tf = parseLogicalScalarLocal(value, name)
  if islogical(value) && isscalar(value)
    tf = value ~= 0;
  elseif isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
    tf = value ~= 0;
  else
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
end
%=============================================================================
function tf = isPositiveIntegerLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) ...
    && isfinite(value) && floor(value) == value && value > 0;
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
