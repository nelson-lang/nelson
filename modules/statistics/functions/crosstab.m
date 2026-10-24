%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = crosstab(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 4);
  [inputs, opts] = parseCrosstabInputsLocal(varargin);
  [codes, labelColumns, labels, keep] = crosstabCodesLocal(inputs, opts.IncludeMissingGroups);
  counts = crosstabCountsLocal(codes, labelColumns, keep);
  varargout{1} = formatCrosstabOutputLocal(counts, labels, opts.OutputFormat);
  if nargout > 1
    varargout{2} = chiSquareLocal(counts);
  end
  if nargout > 2
    varargout{3} = chiSquarePValueLocal(varargout{2}, counts);
  end
  if nargout > 3
    varargout{4} = labels;
  end
end
%=============================================================================
function [inputs, opts] = parseCrosstabInputsLocal(args)
  opts = struct('IncludeMissingGroups', false, 'OutputFormat', 'matrix');
  firstName = findFirstNameLocal(args);
  dataArgs = args(1:firstName - 1);
  if isempty(dataArgs)
    error('Nelson:statistics:leastOneGroupingVariableRequired', 'At least one %s is required.', 'grouping variable');
  end
  if length(dataArgs) == 1 && istable(dataArgs{1})
    dataArgs = tableVariablesLocal(dataArgs{1});
  end
  inputs = dataArgs;
  k = firstName;
  while k <= length(args)
    if k == length(args)
      error('Nelson:statistics:nameValueArgumentFollowedValue', 'Name-value argument must be followed by a value.');
    end
    name = lower(char(args{k}));
    switch name
      case 'includemissinggroups'
        opts.IncludeMissingGroups = logical(args{k + 1});
      case 'outputformat'
        opts.OutputFormat = lower(char(args{k + 1}));
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
end
%=============================================================================
function idx = findFirstNameLocal(args)
  names = {'IncludeMissingGroups', 'OutputFormat'};
  idx = length(args) + 1;
  for k = 1:length(args)
    if ischar(args{k}) || (isstring(args{k}) && isscalar(args{k}))
      text = char(args{k});
      if any(strcmpi(text, names))
        idx = k;
        return
      end
    end
  end
end
%=============================================================================
function inputs = tableVariablesLocal(T)
  st = struct(T);
  names = st.Properties.VariableNames;
  inputs = cell(1, length(names));
  for k = 1:length(names)
    inputs{k} = T.(names{k});
  end
end
%=============================================================================
function [codes, labelColumns, labels, keep] = crosstabCodesLocal(inputs, includeMissing)
  n = numel(inputs{1});
  codes = zeros(n, length(inputs));
  valuesColumns = cell(1, length(inputs));
  labelColumns = cell(1, length(inputs));
  keep = true(n, 1);
  for dim = 1:length(inputs)
    if numel(inputs{dim}) ~= n
      error('Nelson:statistics:allGroupingVariablesSameNumber', 'All grouping variables must have the same number of elements.');
    end
    values = crosstabValuesLocal(inputs{dim});
    valuesColumns{dim} = values;
    missing = missingMaskLocal(inputs{dim});
    if ~includeMissing
      keep = keep & ~missing;
    end
  end
  for dim = 1:length(inputs)
    values = valuesColumns{dim};
    [groups, ic] = groupsAndCodesLocal(values, keep);
    labelColumns{dim} = labelsFromGroupsLocal(groups);
    if isempty(groups)
      codes(:, dim) = 0;
    else
      codes(keep, dim) = ic(:);
    end
  end
  labels = labelsMatrixLocal(labelColumns);
end
%=============================================================================
function labels = labelsMatrixLocal(labelColumns)
  maxLen = 0;
  for k = 1:length(labelColumns)
    maxLen = max(maxLen, length(labelColumns{k}));
  end
  labels = cell(maxLen, length(labelColumns));
  for k = 1:length(labelColumns)
    labels(1:length(labelColumns{k}), k) = labelColumns{k};
  end
end
%=============================================================================
function values = crosstabValuesLocal(x)
  if iscategorical(x)
    values = cellstr(x(:));
  elseif ischar(x)
    values = cellstr(x);
  elseif isstring(x)
    values = cellstr(x(:));
  elseif iscellstr(x)
    values = x(:);
  elseif isnumeric(x) || islogical(x)
    values = x(:);
  else
    error('Nelson:statistics:groupingVariablesVectorsTextArrays', 'Grouping variables must be vectors or text arrays.');
  end
end
%=============================================================================
function mask = missingMaskLocal(x)
  if isnumeric(x)
    mask = isnan(x(:));
  elseif iscategorical(x)
    mask = strcmp(cellstr(x(:)), '<undefined>');
  else
    mask = false(numel(x), 1);
  end
end
%=============================================================================
function [groups, ic] = groupsAndCodesLocal(values, keep)
  if isnumeric(values) || islogical(values)
    [groups, ~, ic] = unique(values(keep));
  else
    [groups, ~, ic] = unique(values(keep), 'stable');
  end
end
%=============================================================================
function labels = labelsFromGroupsLocal(groups)
  if iscell(groups)
    labels = groups(:);
  else
    labels = cell(length(groups), 1);
    for k = 1:length(groups)
      labels{k} = mat2str(groups(k));
    end
  end
end
%=============================================================================
function counts = crosstabCountsLocal(codes, labelColumns, keep)
  dims = zeros(1, size(codes, 2));
  for k = 1:size(codes, 2)
    dims(k) = length(labelColumns{k});
  end
  if length(dims) == 1
    dims = [dims, 1];
  end
  counts = zeros(dims);
  keptRows = find(keep);
  if isempty(keptRows)
    return
  end
  linear = ones(length(keptRows), 1);
  stride = 1;
  for dim = 1:size(codes, 2)
    linear = linear + (codes(keptRows, dim) - 1) * stride;
    stride = stride * dims(dim);
  end
  linear = sort(linear);
  starts = [1; find(diff(linear) ~= 0) + 1];
  stops = [starts(2:end) - 1; length(linear)];
  for k = 1:length(starts)
    counts(linear(starts(k))) = stops(k) - starts(k) + 1;
  end
end
%=============================================================================
function out = formatCrosstabOutputLocal(counts, labels, format)
  switch format
    case 'matrix'
      out = counts;
    case 'table'
      out = crosstabTableLocal(counts, labels);
    case {'stacked-table', 'stacked'}
      out = crosstabStackedLocal(counts, labels);
    otherwise
      error('Nelson:statistics:unsupportedOutputFormat', 'Unsupported %s.', 'output format');
  end
end
%=============================================================================
function T = crosstabTableLocal(counts, labels)
  if ndims(counts) ~= 2
    error('Nelson:statistics:tableOutputFormatSupportedTwo', 'Table output format is supported for two grouping variables.');
  end
  T = array2table(counts);
  rowValues = labels(:, 1);
  T.Group1 = rowValues;
  T = movevars(T, 'Group1', 'Before', 1);
end
%=============================================================================
function T = crosstabStackedLocal(counts, labels)
  n = numel(counts);
  columns = cell(1, ndims(counts));
  [columns{:}] = ind2sub(size(counts), (1:n)');
  values = cell(1, ndims(counts));
  for dim = 1:ndims(counts)
    values{dim} = labels(columns{dim}, dim);
  end
  T = table(values{:}, counts(:), 'VariableNames', stackedNamesLocal(ndims(counts)));
end
%=============================================================================
function names = stackedNamesLocal(n)
  names = cell(1, n + 1);
  for k = 1:n
    names{k} = ['Group', int2str(k)];
  end
  names{end} = 'Count';
end
%=============================================================================
function value = chiSquareLocal(counts)
  if isempty(counts) || sum(counts(:)) == 0
    value = NaN;
    return
  end
  if ndims(counts) ~= 2
    value = NaN;
    return
  end
  total = sum(counts(:));
  expected = sum(counts, 2) * sum(counts, 1) / total;
  valid = expected > 0;
  value = sum(((counts(valid) - expected(valid)) .^ 2) ./ expected(valid));
end
%=============================================================================
function p = chiSquarePValueLocal(chi2, counts)
  if isnan(chi2) || ndims(counts) ~= 2
    p = NaN;
    return
  end
  df = (size(counts, 1) - 1) * (size(counts, 2) - 1);
  p = gammaincUpperLocal(chi2 / 2, df / 2);
end
%=============================================================================
function q = gammaincUpperLocal(x, a)
  if x < 0 || a <= 0
    q = NaN;
  elseif x == 0
    q = 1;
  elseif x < a + 1
    q = 1 - gammaincSeriesLocal(x, a);
  else
    q = gammaincFractionLocal(x, a);
  end
end
%=============================================================================
function p = gammaincSeriesLocal(x, a)
  ap = a;
  del = 1 / a;
  sumValue = del;
  for k = 1:200
    ap = ap + 1;
    del = del * x / ap;
    sumValue = sumValue + del;
    if abs(del) < abs(sumValue) * eps
      break
    end
  end
  p = sumValue * exp(-x + a * log(x) - gammaln(a));
end
%=============================================================================
function q = gammaincFractionLocal(x, a)
  fpmin = realmin / eps;
  b = x + 1 - a;
  c = 1 / fpmin;
  d = 1 / max(b, fpmin);
  h = d;
  for k = 1:200
    an = -k * (k - a);
    b = b + 2;
    d = an * d + b;
    if abs(d) < fpmin
      d = fpmin;
    end
    c = b + an / c;
    if abs(c) < fpmin
      c = fpmin;
    end
    d = 1 / d;
    del = d * c;
    h = h * del;
    if abs(del - 1) < eps
      break
    end
  end
  q = exp(-x + a * log(x) - gammaln(a)) * h;
end
%=============================================================================
