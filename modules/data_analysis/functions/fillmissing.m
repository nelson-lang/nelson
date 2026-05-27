%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, FB] = fillmissing(A, method, varargin)
  narginchk(2, Inf);
  opts = parseFillmissingInputs(A, method, varargin);
  if istable(A)
    [B, FB] = fillTable(A, method, opts);
  else
    [B, FB] = fillArray(A, method, opts.Constant, opts.ConstantProvided, opts.MissingLocations, opts.EndValues, opts.SamplePoints, opts.MaxGap);
  end
  if nargout < 2
    clear FB;
  end
end
%=============================================================================
function opts = parseFillmissingInputs(A, method, args)
  opts = struct();
  opts.Constant = [];
  opts.ConstantProvided = false;
  opts.MissingLocations = [];
  opts.EndValues = 'extrap';
  opts.SamplePoints = [];
  opts.MaxGap = [];
  opts.DataVariables = [];
  opts.ReplaceValues = true;
  opts.Dim = [];
  methodName = '';
  if ischar(method) || (isstring(method) && isscalar(method))
    methodName = lower(char(method));
  end
  if ~strcmp(methodName, 'constant') && ~isempty(args) && isnumeric(args{1}) && isscalar(args{1})
    opts.Dim = args{1};
    args(1) = [];
  end
  if ~isempty(args)
    first = args{1};
    if ~(ischar(first) || (isstring(first) && isscalar(first)))
      opts.Constant = first;
      opts.ConstantProvided = true;
      args(1) = [];
    end
  end
  k = 1;
  while k <= length(args)
    name = args{k};
    if isstring(name)
      name = char(name);
    end
    if ~ischar(name)
      error(_('Name-value argument expected.'));
    end
    if k == length(args)
      error(_('Name-value argument must be followed by a value.'));
    end
    switch lower(name)
      case 'missinglocations'
        opts.MissingLocations = args{k + 1};
      case 'endvalues'
        opts.EndValues = args{k + 1};
      case 'samplepoints'
        opts.SamplePoints = args{k + 1};
      case 'maxgap'
        opts.MaxGap = args{k + 1};
      case 'datavariables'
        opts.DataVariables = args{k + 1};
      case 'replacevalues'
        opts.ReplaceValues = logical(args{k + 1});
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
end
%=============================================================================
function [B, FB] = fillTable(A, method, opts)
  B = A;
  FB = false(height(A), width(A));
  variableNames = A.Properties.VariableNames;
  if isempty(opts.DataVariables)
    dataVars = 1:width(A);
  else
    stA = struct(A);
    dataVars = tableResolveVariables(stA.Properties.VariableNames, opts.DataVariables, stA.Properties.VariableTypes, stA.data);
  end
  filledTable = table();
  filledNames = {};
  for position = 1:length(dataVars)
    v = dataVars(position);
    name = variableNames{v};
    missingLocations = [];
    if ~isempty(opts.MissingLocations)
      if istable(opts.MissingLocations)
        missingLocations = opts.MissingLocations.(name);
      else
        missingLocations = opts.MissingLocations(:, v);
      end
    end
    fillConstant = constantForVariable(opts.Constant, position, length(dataVars));
    [filledValue, filledMask] = fillArray(A.(name), method, fillConstant, opts.ConstantProvided, missingLocations, opts.EndValues, opts.SamplePoints, opts.MaxGap);
    if opts.ReplaceValues
      B.(name) = filledValue;
      FB(:, v) = filledMask(:);
    else
      filledNames{end + 1} = [name, '_filled'];
      filledTable.(filledNames{end}) = filledValue;
      FB(:, position) = filledMask(:);
    end
  end
  if ~opts.ReplaceValues
    B = [A, filledTable];
  end
end
%=============================================================================
function value = constantForVariable(constants, position, nVars)
  value = constants;
  if isempty(constants)
    return
  end
  if iscell(constants)
    if length(constants) == nVars
      value = constants{position};
    else
      value = constants{1};
    end
  elseif ~isscalar(constants) && size(constants, 2) >= position
    value = constants(position);
  end
end
%=============================================================================
function [B, filledMask] = fillArray(A, method, constantValue, constantProvided, missingLocations, endValues, samplePoints, maxGap)
  B = A;
  if isempty(missingLocations)
    missingMask = ismissing(A);
  else
    missingMask = logical(missingLocations);
  end
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  if isempty(missingMask)
    return
  end
  if isvector(A)
    [B, filledMask] = fillVector(A, missingMask, method, constantValue, constantProvided, endValues, samplePoints, maxGap);
    return
  end
  for c = 1:size(A, 2)
    [B(:, c), filledMask(:, c)] = fillVector(A(:, c), missingMask(:, c), method, constantValue, constantProvided, endValues, samplePoints, maxGap);
  end
end
%=============================================================================
function [B, filledMask] = fillVector(A, missingMask, method, constantValue, constantProvided, endValues, samplePoints, maxGap)
  B = A;
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  if isstring(method)
    method = char(method);
  end
  if isa(method, 'function_handle')
    [B, filledMask] = fillFunctionWindow(B, missingMask, method, constantValue, samplePoints, maxGap);
    return
  end
  switch lower(method)
    case 'constant'
      if ~constantProvided
        error(_('Fill value expected.'));
      end
      [B, filledMask] = assignMissing(B, missingMask, constantValue);
    case 'previous'
      [B, filledMask] = fillPrevious(B, missingMask);
    case 'next'
      [B, filledMask] = fillNext(B, missingMask);
    case 'nearest'
      [B, filledMask] = fillNearest(B, missingMask);
    case 'linear'
      [B, filledMask] = fillLinear(B, missingMask, samplePoints, endValues, maxGap);
    case 'movmean'
      [B, filledMask] = fillMoving(B, missingMask, constantValue, @mean);
    case 'movmedian'
      [B, filledMask] = fillMoving(B, missingMask, constantValue, @medianLocal);
    case {'spline', 'pchip', 'makima', 'knn'}
      error(_('Unsupported fill method.'));
    otherwise
      error(_('Unsupported fill method.'));
  end
end
%=============================================================================
function [B, filledMask] = assignMissing(B, missingMask, value)
  filledMask = missingMask;
  if iscell(B)
    if ~(ischar(value) || (isstring(value) && isscalar(value)))
      error(_('Invalid fill constant type for table variable.'));
    end
    if isstring(value)
      value = char(value);
    end
    B(missingMask) = {value};
  else
    B(missingMask) = value;
  end
end
%=============================================================================
function [B, filledMask] = fillPrevious(B, missingMask)
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  for k = 1:numel(B)
    if missingMask(k) && k > 1 && ~missingMask(k - 1)
      B(k) = B(k - 1);
      missingMask(k) = false;
      filledMask(k) = true;
    end
  end
end
%=============================================================================
function [B, filledMask] = fillNext(B, missingMask)
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  for k = numel(B):-1:1
    if missingMask(k) && k < numel(B) && ~missingMask(k + 1)
      B(k) = B(k + 1);
      missingMask(k) = false;
      filledMask(k) = true;
    end
  end
end
%=============================================================================
function [B, filledMask] = fillNearest(B, missingMask)
  [prevB, prevMask] = fillPrevious(B, missingMask);
  [nextB, nextMask] = fillNext(B, missingMask);
  B = prevB;
  filledMask = prevMask;
  missingIdx = find(missingMask);
  knownIdx = find(~missingMask);
  for k = missingIdx(:)'
    if isempty(knownIdx)
      continue
    end
    [~, pos] = min(abs(knownIdx - k));
    B(k) = B(knownIdx(pos));
    filledMask(k) = true;
  end
  B(nextMask & ~prevMask) = nextB(nextMask & ~prevMask);
end
%=============================================================================
function [B, filledMask] = fillLinear(B, missingMask, samplePoints, endValues, maxGap)
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  if isempty(samplePoints)
    x = (1:numel(B))';
  else
    x = samplePoints(:);
  end
  values = double(B(:));
  known = ~missingMask(:);
  if sum(known) < 2
    return
  end
  missingRuns = consecutiveRuns(find(missingMask(:)));
  for r = 1:size(missingRuns, 1)
    idx = missingRuns(r, 1):missingRuns(r, 2);
    left = find(known(1:idx(1) - 1), 1, 'last');
    rightRel = find(known(idx(end) + 1:end), 1, 'first');
    if isempty(left) || isempty(rightRel)
      if shouldFillEnd(endValues)
        [B, endFilled] = fillEndValues(B, missingMask, idx, endValues);
        filledMask(idx) = endFilled(idx);
      end
      continue
    end
    right = idx(end) + rightRel;
    gap = x(right) - x(left);
    if ~isempty(maxGap) && gap > maxGap
      continue
    end
    values(idx) = values(left) + (values(right) - values(left)) .* (x(idx) - x(left)) ./ gap;
    filledMask(idx) = true;
  end
  B(:) = values;
end
%=============================================================================
function tf = shouldFillEnd(endValues)
  tf = ischar(endValues) && ~strcmpi(endValues, 'none');
end
%=============================================================================
function [B, filledMask] = fillEndValues(B, missingMask, idx, endValues)
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  if isstring(endValues)
    endValues = char(endValues);
  end
  if ischar(endValues)
    switch lower(endValues)
      case {'extrap', 'nearest'}
        known = find(~missingMask(:));
        if isempty(known)
          return
        end
        for k = idx
          [~, pos] = min(abs(known - k));
          B(k) = B(known(pos));
          filledMask(k) = true;
        end
      case 'previous'
        [B, filledMask] = fillPrevious(B, missingMask);
      case 'next'
        [B, filledMask] = fillNext(B, missingMask);
      case 'none'
        return
      otherwise
        error(_('Unsupported EndValues method.'));
    end
  else
    B(idx) = endValues;
    filledMask(idx) = true;
  end
end
%=============================================================================
function runs = consecutiveRuns(idx)
  if isempty(idx)
    runs = zeros(0, 2);
    return
  end
  if isscalar(idx)
    runs = [idx, idx];
    return
  end
  starts = idx([true; diff(idx(:)) > 1]);
  ends = idx([diff(idx(:)) > 1; true]);
  runs = [starts(:), ends(:)];
end
%=============================================================================
function [B, filledMask] = fillMoving(B, missingMask, window, fun)
  if isempty(window)
    error(_('Window size expected.'));
  end
  if isscalar(window)
    before = floor((window - 1) / 2);
    after = window - before - 1;
  else
    before = window(1);
    after = window(2);
  end
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  for k = find(missingMask(:))'
    lo = max(1, k - before);
    hi = min(numel(B), k + after);
    sample = double(B(lo:hi));
    sample = sample(~missingMask(lo:hi));
    if ~isempty(sample)
      B(k) = fun(sample);
      filledMask(k) = true;
    end
  end
end
%=============================================================================
function [B, filledMask] = fillFunctionWindow(B, missingMask, fun, window, samplePoints, maxGap)
  if isempty(window)
    error(_('Window size expected.'));
  end
  if isempty(samplePoints)
    x = (1:numel(B))';
  else
    x = samplePoints(:);
  end
  if isscalar(window)
    before = floor((window - 1) / 2);
    after = window - before - 1;
  else
    before = window(1);
    after = window(2);
  end
  filledMask = false(size(missingMask, 1), size(missingMask, 2));
  for k = find(missingMask(:))'
    lo = max(1, k - before);
    hi = min(numel(B), k + after);
    sampleIdx = lo:hi;
    sampleIdx = sampleIdx(~missingMask(sampleIdx));
    if isempty(sampleIdx)
      continue
    end
    B(k) = fun(double(B(sampleIdx)), x(sampleIdx), x(k));
    filledMask(k) = true;
  end
end
%=============================================================================
function value = medianLocal(values)
  values = sort(values(:));
  n = length(values);
  if n == 0
    value = NaN;
  elseif mod(n, 2) == 1
    value = values((n + 1) / 2);
  else
    value = (values(n / 2) + values(n / 2 + 1)) / 2;
  end
end
%=============================================================================
