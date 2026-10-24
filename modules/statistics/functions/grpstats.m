%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = grpstats(X, group, whichstats, varargin)
  narginchk(2, Inf);
  if nargin < 3
    useDefaultStats = true;
  else
    useDefaultStats = isempty(whichstats) || isnumeric(whichstats);
  end
  if useDefaultStats && ~istable(X)
    whichstats = defaultStatsLocal(X);
  end
  opts = parseOptionsLocal(varargin);
  if istable(X)
    nargoutchk(0, 1);
    if useDefaultStats
      whichstats = defaultTableStatsLocal(X, group, opts);
    end
    stats = normalizeStatsLocal(whichstats);
    varargout{1} = grpstatsTableLocal(X, group, stats, opts);
  else
    stats = normalizeStatsLocal(whichstats);
    outputs = grpstatsArrayLocal(X, group, stats, opts);
    for k = 1:max(nargout, 1)
      varargout{k} = outputs{min(k, length(outputs))};
    end
    if nargout == 0
      disp(varargout{1});
    end
  end
end
%=============================================================================
function stats = defaultTableStatsLocal(X, groupvars, opts)
  st = struct(X);
  names = st.Properties.VariableNames;
  groupIdx = resolveVariablesLocal(names, groupvars, st.Properties.VariableTypes, st.data);
  dataIdx = resolveDataVarsLocal(names, groupIdx, opts.DataVars, st.Properties.VariableTypes, st.data);
  if isempty(dataIdx)
    stats = 'numel';
    return
  end
  stats = 'mean';
  for k = dataIdx(:)'
    value = st.data.(names{k});
    if ~(isnumeric(value) || islogical(value))
      stats = 'numel';
      return
    end
  end
end
%=============================================================================
function stats = defaultStatsLocal(X)
  if istable(X)
    st = struct(X);
    stats = 'mean';
    for k = 1:length(st.Properties.VariableNames)
      value = st.data.(st.Properties.VariableNames{k});
      if ~(isnumeric(value) || islogical(value))
        stats = 'numel';
        return
      end
    end
  else
    stats = 'mean';
  end
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Alpha', 0.05, 'DataVars', []);
  k = 1;
  while k <= length(args)
    if k == length(args)
      error('Nelson:statistics:nameValueArgumentFollowedValue', 'Name-value argument must be followed by a value.');
    end
    name = lower(char(args{k}));
    switch name
      case 'alpha'
        opts.Alpha = args{k + 1};
      case 'datavars'
        opts.DataVars = args{k + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  if ~(isnumeric(opts.Alpha) && isscalar(opts.Alpha) && opts.Alpha > 0 && opts.Alpha < 1)
    error('Nelson:statistics:alphaScalarBetween01', 'Alpha must be a scalar between 0 and 1.');
  end
end
%=============================================================================
function stats = normalizeStatsLocal(whichstats)
  if isa(whichstats, 'function_handle')
    stats = {whichstats};
  elseif ischar(whichstats)
    stats = {lower(whichstats)};
  elseif isstring(whichstats)
    stats = cellstr(lower(whichstats(:)));
  elseif iscell(whichstats)
    stats = whichstats(:)';
    for k = 1:length(stats)
      if isstring(stats{k})
        stats{k} = char(stats{k});
      end
      if ischar(stats{k})
        stats{k} = lower(stats{k});
      end
    end
  else
    error('Nelson:statistics:statisticsNamesFunctionHandles', 'Statistics must be names or function handles.');
  end
end
%=============================================================================
function outputs = grpstatsArrayLocal(X, group, stats, opts)
  if ~(isnumeric(X) || islogical(X))
    error('Nelson:statistics:xNumericLogicalArrayInput', 'X must be numeric or logical for array input.');
  end
  if isvector(X)
    X = X(:);
  end
  if size(X, 1) ~= groupLengthLocal(group)
    error('Nelson:statistics:xGroupSameNumberRows', '%s must have the same number of rows.', 'X and group');
  end
  [uniqueKeys, ia, ic, labels] = arrayGroupsLocal(group);
  nGroups = length(uniqueKeys);
  [order, edges] = groupOrderLocal(ic, nGroups);
  outputs = cell(1, length(stats));
  for s = 1:length(stats)
    if ischar(stats{s}) && strcmp(stats{s}, 'gname')
      outputs{s} = labels;
    else
      outputs{s} = applyArrayStatLocal(X, order, edges, nGroups, stats{s}, opts.Alpha);
    end
  end
end
%=============================================================================
function [uniqueKeys, ia, ic, labels] = arrayGroupsLocal(group)
  if (isnumeric(group) || islogical(group)) && isvector(group)
    [values, ia, ic] = unique(group(:), 'stable');
    uniqueKeys = cell(length(values), 1);
    labels = cell(length(values), 1);
    for k = 1:length(values)
      text = mat2str(values(k));
      uniqueKeys{k} = text;
      labels{k} = text;
    end
  else
    [keys, rowLabels] = groupKeysLocal(group);
    [uniqueKeys, ia, ic] = uniqueStableLocal(keys);
    labels = rowLabels(ia, :);
  end
end
%=============================================================================
function T = grpstatsTableLocal(Tin, groupvars, stats, opts)
  st = struct(Tin);
  names = st.Properties.VariableNames;
  groupIdx = resolveVariablesLocal(names, groupvars, st.Properties.VariableTypes, st.data);
  dataIdx = resolveDataVarsLocal(names, groupIdx, opts.DataVars, st.Properties.VariableTypes, st.data);
  keys = rowKeysLocal(st, height(Tin), groupIdx);
  [uniqueKeys, ia, ic] = uniqueStableLocal(keys);
  rows = ia;
  nGroups = length(uniqueKeys);
  [order, edges] = groupOrderLocal(ic, nGroups);
  groupCounts = diff(edges);
  T = Tin(rows, names(groupIdx));
  T.GroupCount = groupCounts;
  for s = 1:length(stats)
    if ischar(stats{s}) && strcmp(stats{s}, 'gname')
      continue
    end
    for j = 1:length(dataIdx)
      name = names{dataIdx(j)};
      values = tableColumnRowsLocal(st.data.(name), 1:height(Tin));
      column = applyArrayStatLocal(values, order, edges, nGroups, stats{s}, opts.Alpha);
      T.([statNameLocal(stats{s}), '_', name]) = column;
    end
  end
end
%=============================================================================
function out = applyArrayStatLocal(X, order, edges, nGroups, stat, alpha)
  out = [];
  for g = 1:nGroups
    rows = order(edges(g) + 1:edges(g + 1));
    value = applyOneStatLocal(X(rows, :), stat, alpha);
    if isempty(out)
      out = zeros(nGroups, size(value, 2));
    end
    out(g, :) = value;
  end
end
%=============================================================================
function [order, edges] = groupOrderLocal(ic, nGroups)
  % Group rows in a single pass: a stable sort of the group indices lays each
  % group's rows out contiguously (ascending, matching find(ic == g)), and the
  % cumulative counts give each group's slice. Replaces per-group find(ic == g).
  [~, order] = sort(ic(:));
  counts = accumarray(ic(:), 1, [nGroups, 1]);
  edges = [0; cumsum(counts)];
end
%=============================================================================
function value = applyOneStatLocal(data, stat, alpha)
  if isa(stat, 'function_handle')
    value = stat(data);
    return
  end
  switch stat
    case 'mean'
      value = mean(data, 1);
    case 'sem'
      value = std(data, 0, 1) ./ sqrt(size(data, 1));
    case 'std'
      value = std(data, 0, 1);
    case 'var'
      value = var(data, 0, 1);
    case 'min'
      value = min(data, [], 1);
    case 'max'
      value = max(data, [], 1);
    case 'range'
      value = max(data, [], 1) - min(data, [], 1);
    case 'median'
      value = median(data, 1);
    case 'mode'
      value = mode(data, 1);
    case 'numel'
      value = repmat(size(data, 1), 1, size(data, 2));
    case 'meanci'
      value = intervalLocal(data, alpha, false);
    case 'predci'
      value = intervalLocal(data, alpha, true);
    otherwise
      error('Nelson:statistics:unsupportedSummaryStatistic', 'Unsupported %s.', 'summary statistic');
  end
end
%=============================================================================
function value = intervalLocal(data, alpha, prediction)
  n = size(data, 1);
  m = mean(data, 1);
  s = std(data, 0, 1);
  if n <= 1
    halfWidth = Inf(size(m));
  else
    z = normalInvLocal(1 - alpha / 2);
    factor = 1 / sqrt(n);
    if prediction
      factor = sqrt(1 + 1 / n);
    end
    halfWidth = z * s * factor;
  end
  value = [m - halfWidth, m + halfWidth];
end
%=============================================================================
function z = normalInvLocal(p)
  z = norminv(p, 0, 1);
end
%=============================================================================
function n = groupLengthLocal(group)
  if iscell(group) && ~(iscellstr(group) && isvector(group))
    n = numel(group{1});
  else
    n = numel(group);
  end
end
%=============================================================================
function [keys, labels] = groupKeysLocal(group)
  if iscell(group) && ~(iscellstr(group) && isvector(group))
    cols = group;
  else
    cols = {group};
  end
  n = numel(cols{1});
  labels = cell(n, length(cols));
  keys = cell(n, 1);
  for r = 1:n
    key = '';
    for c = 1:length(cols)
      value = groupValueLabelLocal(cols{c}, r);
      labels{r, c} = value;
      key = [key, '#', int2str(c), '=', value, ';'];
    end
    keys{r} = key;
  end
end
%=============================================================================
function label = groupValueLabelLocal(values, row)
  if iscategorical(values)
    allLabels = cellstr(values(:));
    label = allLabels{row};
  elseif iscellstr(values)
    flat = values(:);
    label = flat{row};
  elseif isstring(values)
    flat = cellstr(values(:));
    label = flat{row};
  elseif isnumeric(values) || islogical(values)
    flat = values(:);
    label = mat2str(flat(row));
  else
    error('Nelson:statistics:unsupportedGroupingVariable', 'Unsupported %s.', 'grouping variable');
  end
end
%=============================================================================
function idx = resolveDataVarsLocal(names, groupIdx, vars, types, data)
  if isempty(vars)
    idx = [];
    for k = 1:length(names)
      if ~any(groupIdx == k)
        value = data.(names{k});
        if isnumeric(value) || islogical(value)
          idx(end + 1) = k;
        end
      end
    end
  else
    idx = resolveVariablesLocal(names, vars, types, data);
  end
end
%=============================================================================
function idx = resolveVariablesLocal(names, vars, types, data)
  if nargin < 3
    types = [];
  end
  if nargin < 4
    data = struct();
  end
  if islogical(vars)
    idx = find(vars);
    return
  elseif isnumeric(vars)
    idx = vars;
    return
  elseif isstring(vars)
    vars = cellstr(vars);
  elseif ischar(vars)
    vars = {vars};
  elseif strcmp(class(vars), 'vartype')
    idx = find(strcmp(types, vars.TypeName));
    return
  elseif isa(vars, 'function_handle')
    idx = functionVarSelectLocal(vars, names, types, data);
    return
  end
  idx = zeros(1, length(vars));
  for k = 1:length(vars)
    found = find(strcmp(names, vars{k}), 1);
    if isempty(found)
      error('Nelson:statistics:unrecognizedTableVariableName', 'Unrecognized table variable name.');
    end
    idx(k) = found;
  end
end
%=============================================================================
function idx = functionVarSelectLocal(fcn, names, types, data)
  idx = [];
  for k = 1:length(names)
    try
      tf = fcn(types(k));
    catch
      tf = fcn(data.(names{k}));
    end
    if tf
      idx(end + 1) = k;
    end
  end
end
%=============================================================================
function keys = rowKeysLocal(st, nRows, vars)
  % Build one text key per row. Tokens are produced per column (type dispatched
  % once) with inline concatenation and vectorised builtins; the former per-row
  % st.data.(name) access and recursive valueKeyLocal call were quadratic.
  keys = repmat({''}, nRows, 1);
  for j = 1:length(vars)
    column = st.data.(st.Properties.VariableNames{vars(j)});
    prefix = ['#', int2str(j), '='];
    tokens = columnTokensLocal(column, nRows);
    for r = 1:nRows
      keys{r} = [keys{r}, prefix, tokens{r}, ';'];
    end
  end
end
%=============================================================================
function tokens = columnTokensLocal(column, nRows)
  tokens = cell(nRows, 1);
  if iscellstr(column)
    for r = 1:nRows
      tokens{r} = ['cell:char:', column{r}];
    end
  elseif ischar(column)
    for r = 1:nRows
      tokens{r} = ['char:', column(r, :)];
    end
  elseif isstring(column)
    for r = 1:nRows
      tokens{r} = ['string:', char(column(r))];
    end
  elseif isnumeric(column) || islogical(column)
    className = class(column);
    for r = 1:nRows
      tokens{r} = [className, ':', mat2str(column(r, :))];
    end
  else
    for r = 1:nRows
      tokens{r} = valueKeyLocal(tableColumnRowsLocal(column, r));
    end
  end
end
%=============================================================================
function value = tableColumnRowsLocal(value, rows)
  if isvector(value)
    value = value(rows, :);
  else
    idx = repmat({':'}, 1, ndims(value) - 1);
    value = value(rows, idx{:});
  end
end
%=============================================================================
function key = valueKeyLocal(value)
  if ischar(value)
    key = ['char:', value];
  elseif isstring(value)
    key = ['string:', char(value)];
  elseif iscell(value)
    key = ['cell:', valueKeyLocal(value{1})];
  elseif isnumeric(value) || islogical(value)
    key = [class(value), ':', mat2str(value)];
  else
    key = [class(value), ':', char(string(value))];
  end
end
%=============================================================================
function [values, ia, ic] = uniqueStableLocal(valuesIn)
  % Delegate to the builtin (first-occurrence order); the former element-by-
  % element scan of a growing list was quadratic.
  [values, ia, ic] = unique(valuesIn(:), 'stable');
  values = values(:);
  ia = ia(:);
  ic = ic(:);
end
%=============================================================================
function name = statNameLocal(stat)
  if isa(stat, 'function_handle')
    name = 'fun';
  else
    name = char(stat);
  end
end
%=============================================================================
