%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = groupcounts(T, varargin)
  narginchk(1, Inf);
  if istable(T)
    nargoutchk(0, 1);
    if isempty(varargin)
      error(_('Grouping variables expected.'));
    end
    groupVars = varargin{1};
    args = varargin(2:end);
    varargout{1} = groupcountsTable(T, groupVars, args);
  else
    [counts, groups, percent] = groupcountsArray(T, varargin);
    varargout{1} = counts;
    if nargout > 1
      varargout{2} = groups;
    end
    if nargout > 2
      varargout{3} = percent;
    end
  end
end
%=============================================================================
function G = groupcountsTable(T, groupVars, args)
  opts = parseGroupcountsOptions(args);
  if opts.GroupBinsProvided
    error(_('Group bins are not supported for table groupcounts.'));
  end
  st = struct(T);
  idx = resolveVariablesLocal(st.Properties.VariableNames, groupVars, st.Properties.VariableTypes, st.data);
  if isempty(idx)
    counts = height(T);
    percent = 100;
    G = table(counts, percent, 'VariableNames', {'GroupCount', 'Percent'});
    return
  end
  names = st.Properties.VariableNames;
  keys = rowKeysLocal(st, height(T), idx);
  missingRows = any(ismissing(T(:, names(idx))), 2);
  if ~opts.IncludeMissingGroups
    keep = ~missingRows;
    keys = keys(keep);
    sourceRows = find(keep);
  else
    sourceRows = (1:height(T))';
  end
  [uniqueKeys, ia] = uniqueStableLocal(keys);
  rows = sourceRows(ia);
  counts = zeros(length(uniqueKeys), 1);
  for k = 1:length(uniqueKeys)
    counts(k) = sum(strcmp(keys, uniqueKeys{k}));
  end
  total = sum(counts);
  if total == 0
    percent = counts;
  else
    percent = counts * 100 / total;
  end
  G = T(rows, names(idx));
  G.GroupCount = counts;
  G.Percent = percent;
end
%=============================================================================
function [counts, groups, percent] = groupcountsArray(X, args)
  opts = parseGroupcountsOptions(args);
  if opts.GroupBinsProvided
    error(_('Group bins are not supported.'));
  end
  if iscell(X)
    n = numel(X{1});
    keys = cell(n, 1);
    for r = 1:n
      parts = cell(1, length(X));
      for c = 1:length(X)
        parts{c} = valueKeyLocal(X{c}(r));
      end
      keys{r} = strjoin(parts, '|');
    end
    groupSource = X;
  else
    n = size(X, 1);
    keys = cell(n, 1);
    for r = 1:n
      keys{r} = valueKeyLocal(X(r, :));
    end
    groupSource = X;
  end
  [uniqueKeys, ia] = uniqueStableLocal(keys);
  counts = zeros(length(uniqueKeys), 1);
  for k = 1:length(uniqueKeys)
    counts(k) = sum(strcmp(keys, uniqueKeys{k}));
  end
  total = sum(counts);
  if total == 0
    percent = counts;
  else
    percent = counts * 100 / total;
  end
  if iscell(groupSource)
    groups = cell(1, length(groupSource));
    for c = 1:length(groupSource)
      groups{c} = groupSource{c}(ia);
    end
  else
    groups = groupSource(ia, :);
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
  if nargin < 2 || isempty(vars) || (ischar(vars) && strcmp(vars, ':'))
    idx = 1:length(names);
    return
  end
  if strcmp(class(vars), 'vartype')
    idx = find(strcmp(types, vars.TypeName));
    return
  end
  if isa(vars, 'function_handle')
    idx = [];
    for k = 1:length(names)
      try
        tf = vars(types(k));
      catch
        tf = vars(data.(names{k}));
      end
      if tf
        idx(end + 1) = k;
      end
    end
    return
  end
  if islogical(vars)
    idx = find(vars);
    return
  end
  if isnumeric(vars)
    idx = vars;
    return
  end
  if isstring(vars)
    vars = cellstr(vars);
  elseif ischar(vars)
    vars = {vars};
  end
  idx = zeros(1, length(vars));
  for k = 1:length(vars)
    found = find(strcmp(names, vars{k}), 1);
    if isempty(found)
      error(_('Unrecognized table variable name.'));
    end
    idx(k) = found;
  end
end
%=============================================================================
function keys = rowKeysLocal(st, nRows, vars)
  keys = cell(nRows, 1);
  for r = 1:nRows
    key = '';
    for j = 1:length(vars)
      name = st.Properties.VariableNames{vars(j)};
      key = [key, '#', int2str(j), '=', valueKeyLocal(columnRowsLocal(st.data.(name), r)), ';'];
    end
    keys{r} = key;
  end
end
%=============================================================================
function value = columnRowsLocal(value, rows)
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
    if isempty(value)
      key = 'cell:[]';
    else
      key = ['cell:', valueKeyLocal(value{1})];
    end
  elseif isnumeric(value) || islogical(value)
    key = [class(value), ':', mat2str(value)];
  else
    try
      key = [class(value), ':', mat2str(value)];
    catch
      key = [class(value), ':', char(string(value))];
    end
  end
end
%=============================================================================
function [values, ia, ic] = uniqueStableLocal(valuesIn)
  values = {};
  ia = [];
  ic = zeros(size(valuesIn));
  for k = 1:length(valuesIn)
    found = find(strcmp(values, valuesIn{k}), 1);
    if isempty(found)
      values{end + 1} = valuesIn{k};
      ia(end + 1) = k;
      found = length(values);
    end
    ic(k) = found;
  end
  values = values(:);
  ia = ia(:);
end
%=============================================================================
function opts = parseGroupcountsOptions(args)
  opts = struct();
  opts.IncludeMissingGroups = true;
  opts.IncludeEmptyGroups = false;
  opts.IncludedEdge = 'left';
  opts.GroupBinsProvided = false;
  if ~isempty(args)
    first = args{1};
    if ~(ischar(first) || (isstring(first) && isscalar(first)))
      opts.GroupBinsProvided = true;
      args(1) = [];
    end
  end
  k = 1;
  while k <= length(args)
    name = args{k};
    if isstring(name)
      name = char(name);
    end
    if k == length(args)
      error(_('Name-value argument must be followed by a value.'));
    end
    switch lower(name)
      case 'includemissinggroups'
        opts.IncludeMissingGroups = logical(args{k + 1});
      case 'includeemptygroups'
        opts.IncludeEmptyGroups = logical(args{k + 1});
      case 'includededge'
        opts.IncludedEdge = char(args{k + 1});
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
end
%=============================================================================
