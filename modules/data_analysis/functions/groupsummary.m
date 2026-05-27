%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function G = groupsummary(T, groupVars, method, dataVars)
  narginchk(2, 4);
  mustBeA(T, 'table', 1);
  if nargin < 3 || isempty(method)
    method = 'count';
  end
  st = struct(T);
  names = st.Properties.VariableNames;
  if nargin < 4
    dataVars = setdiffIndicesLocal(1:width(T), resolveVariablesLocal(names, groupVars, st.Properties.VariableTypes, st.data));
  else
    dataVars = resolveVariablesLocal(names, dataVars, st.Properties.VariableTypes, st.data);
  end
  groupIdx = resolveVariablesLocal(names, groupVars, st.Properties.VariableTypes, st.data);
  keys = rowKeysLocal(st, height(T), groupIdx);
  uniqueKeys = uniqueStableLocal(keys);
  rows = zeros(length(uniqueKeys), 1);
  groupCounts = zeros(length(uniqueKeys), 1);
  values = cell(1, length(dataVars));
  outNames = cell(1, length(dataVars));
  for g = 1:length(uniqueKeys)
    found = find(strcmp(keys, uniqueKeys{g}));
    rows(g) = found(1);
    groupCounts(g) = length(found);
    for j = 1:length(dataVars)
      column = T.(names{dataVars(j)});
      data = columnRowsLocal(column, found);
      values{j} = appendGroupValue(values{j}, applyGroupMethod(data, method));
      outNames{j} = [char(method), '_', names{dataVars(j)}];
    end
  end
  G = T(rows, names(groupIdx));
  G.GroupCount = groupCounts;
  for j = 1:length(values)
    G.(outNames{j}) = values{j};
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
function idx = setdiffIndicesLocal(allIdx, removeIdx)
  idx = [];
  for k = allIdx(:)'
    if ~any(removeIdx == k)
      idx(end + 1) = k;
    end
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
function value = applyGroupMethod(data, method)
  if isa(method, 'function_handle')
    value = method(data);
    return
  end
  switch lower(char(method))
    case 'count'
      value = size(data, 1);
    case 'sum'
      value = sum(data);
    case 'mean'
      value = mean(data);
    case 'min'
      value = min(data);
    case 'max'
      value = max(data);
    otherwise
      error(_('Unsupported summary method.'));
  end
end
%=============================================================================
function values = appendGroupValue(values, value)
  if isempty(values)
    values = value;
  else
    values = [values; value];
  end
end
%=============================================================================
