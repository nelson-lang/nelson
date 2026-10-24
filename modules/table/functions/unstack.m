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
    error('Nelson:table:invalidSyntax:oneDataVariableOneIndicatorVariable', 'One data variable and one indicator variable expected.');
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
        error('Nelson:table:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  indicators = T.(indicatorName);
  if isstring(indicators)
    indicatorNames = cellstr(indicators);
  elseif iscellstr(indicators)
    indicatorNames = indicators(:);
  else
    error('Nelson:table:invalidType:indicatorVariableContainText', 'Indicator variable must contain text.');
  end
  uniqueMeasures = unique(indicatorNames, 'stable');
  newNames = makeUniqueNamesLocal(uniqueMeasures');
  nMeasures = numel(uniqueMeasures);
  [~, measureCol] = ismember(indicatorNames, uniqueMeasures);
  % ismember on text returns a row; force a column to match groupIdx below.
  measureCol = measureCol(:);
  groupKeys = rowKeysLocal(st, height(T), groupingVars);
  [~, firstGroupRow, groupIdx] = unique(groupKeys, 'stable');
  groupIdx = groupIdx(:);
  nGroups = numel(firstGroupRow);
  % First input row for each (group, measure) pair, 0 when the pair is absent.
  % Assigning in reverse row order leaves the earliest row as the winner.
  nRows = height(T);
  slotId = (groupIdx - 1) * nMeasures + measureCol;
  firstRowForSlot = zeros(nGroups * nMeasures, 1);
  firstRowForSlot(slotId(nRows:-1:1)) = (nRows:-1:1)';
  U = table();
  groupingNames = names(groupingVars);
  for k = 1:numel(groupingVars)
    U.(groupingNames{k}) = columnRowsLocal(st.data.(groupingNames{k}), firstGroupRow);
  end
  dataColumn = st.data.(dataName);
  for m = 1:nMeasures
    sourceRows = firstRowForSlot((0:nGroups - 1)' * nMeasures + m);
    present = sourceRows > 0;
    columnValues = defaultValueLocal(dataColumn, nGroups);
    if any(present)
      columnValues(present, :) = columnRowsLocal(dataColumn, sourceRows(present));
    end
    U.(newNames{m}) = columnValues;
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
        error('Nelson:table:unrecognizedVariable', 'Unrecognized table variable name.');
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
  % Build one text key per row. Tokens are produced per column (type dispatched
  % once) with inline concatenation and vectorised builtins; calling the
  % recursive valueKeyLocal once per row would be quadratic.
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
  elseif iscell(column)
    for r = 1:nRows
      tokens{r} = ['cell:', valueKeyLocal(column{r})];
    end
  else
    className = class(column);
    for r = 1:nRows
      tokens{r} = className;
    end
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
