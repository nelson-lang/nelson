%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function U = unstack(T, dataVar, indicatorVar, varargin)
  narginchk(3, Inf);
  mustBeA(T, 'table', 1);
  st = struct(T);
  names = st.Properties.VariableNames;
  dataIdx = resolveVariablesLocal(names, dataVar);
  indicatorIdx = resolveVariablesLocal(names, indicatorVar);
  if length(dataIdx) ~= 1 || length(indicatorIdx) ~= 1
    error(_('One data variable and one indicator variable expected.'));
  end
  dataName = names{dataIdx};
  indicatorName = names{indicatorIdx};
  groupingVars = setdiffIndicesLocal(1:width(T), [dataIdx, indicatorIdx]);
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case 'groupingvariables'
        groupingVars = resolveVariablesLocal(names, varargin{k + 1});
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  indicators = T.(indicatorName);
  if isstring(indicators)
    indicatorNames = cellstr(indicators);
  elseif iscellstr(indicators)
    indicatorNames = indicators(:);
  else
    error(_('Indicator variable must contain text.'));
  end
  newNames = makeUniqueNamesLocal(uniqueStableLocal(indicatorNames)');
  groupKeys = rowKeysLocal(st, height(T), groupingVars);
  uniqueGroups = uniqueStableLocal(groupKeys);
  rows = cell(length(uniqueGroups), 1);
  for g = 1:length(uniqueGroups)
    sourceRows = find(strcmp(groupKeys, uniqueGroups{g}));
    row = T(sourceRows(1), names(groupingVars));
    for n = 1:length(newNames)
      match = sourceRows(strcmp(indicatorNames(sourceRows), newNames{n}));
      if isempty(match)
        row.(newNames{n}) = defaultValueLocal(st.data.(dataName), 1);
      else
        row.(newNames{n}) = columnRowsLocal(st.data.(dataName), match(1));
      end
    end
    rows{g} = row;
  end
  U = rows{1};
  for g = 2:length(rows)
    U = [U; rows{g}];
  end
end
%=============================================================================
function idx = resolveVariablesLocal(names, vars)
  if isempty(vars) || (ischar(vars) && strcmp(vars, ':'))
    idx = 1:length(names);
  elseif islogical(vars)
    idx = find(vars);
  elseif isnumeric(vars)
    idx = vars;
  else
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
    key = class(value);
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
function values = uniqueStableLocal(valuesIn)
  values = {};
  for k = 1:length(valuesIn)
    if isempty(find(strcmp(values, valuesIn{k}), 1))
      values{end + 1} = valuesIn{k};
    end
  end
  values = values(:);
end
%=============================================================================
function names = makeUniqueNamesLocal(names)
  if ischar(names)
    names = {names};
  elseif isstring(names)
    names = cellstr(names);
  end
  names = names(:)';
  used = {};
  for k = 1:length(names)
    base = names{k};
    candidate = base;
    suffix = 1;
    while any(strcmp(used, candidate))
      candidate = [base, '_', num2str(suffix)];
      suffix = suffix + 1;
    end
    names{k} = candidate;
    used{end + 1} = candidate;
  end
end
%=============================================================================
function value = defaultValueLocal(sample, nRows)
  sz = size(sample);
  sz(1) = nRows;
  if isnumeric(sample)
    if isfloat(sample)
      value = NaN(sz);
      value = cast(value, class(sample));
    else
      value = zeros(sz, class(sample));
    end
  elseif islogical(sample)
    value = false(sz);
  elseif isstring(sample)
    value = strings(sz);
  elseif iscell(sample)
    value = cell(sz);
  else
    value = cell(nRows, 1);
  end
end
%=============================================================================
