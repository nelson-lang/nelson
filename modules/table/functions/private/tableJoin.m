%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = tableJoin(left, right, joinType, varargin)
  mustBeA(left, 'table', 1);
  mustBeA(right, 'table', 2);
  opts = parseJoinOptions(left, right, joinType, varargin);
  leftSt = struct(left);
  rightSt = struct(right);
  leftVariableNames = leftSt.Properties.VariableNames;
  rightVariableNames = rightSt.Properties.VariableNames;
  leftKeys = [];
  leftVars = opts.LeftKeys;
  if ischar(leftVars)
    leftVars = {leftVars};
  elseif isstring(leftVars)
    leftVars = cellstr(leftVars);
  end
  if isnumeric(leftVars)
    leftKeys = leftVars;
  elseif islogical(leftVars)
    leftKeys = find(leftVars);
  else
    for lk = 1:length(leftVars)
      found = find(strcmp(leftVariableNames, leftVars{lk}), 1);
      if isempty(found)
        error(_('Unrecognized table variable name.'));
      end
      leftKeys(end + 1) = found;
    end
  end
  rightKeys = [];
  rightVars = opts.RightKeys;
  if ischar(rightVars)
    rightVars = {rightVars};
  elseif isstring(rightVars)
    rightVars = cellstr(rightVars);
  end
  if isnumeric(rightVars)
    rightKeys = rightVars;
  elseif islogical(rightVars)
    rightKeys = find(rightVars);
  else
    for rk = 1:length(rightVars)
      found = find(strcmp(rightVariableNames, rightVars{rk}), 1);
      if isempty(found)
        error(_('Unrecognized table variable name.'));
      end
      rightKeys(end + 1) = found;
    end
  end
  if length(leftKeys) ~= length(rightKeys)
    error(_('Key lists must have the same length.'));
  end
  leftNames = leftVariableNames;
  rightNames = rightVariableNames;
  leftKeyNames = leftNames(leftKeys);
  rightKeyNames = rightNames(rightKeys);
  if isempty(opts.LeftVariables)
    leftVariables = 1:width(left);
  else
    leftVariables = resolveVariablesLocal(leftNames, opts.LeftVariables);
  end
  if isempty(opts.RightVariables)
    rightVariables = 1:width(right);
  else
    rightVariables = resolveVariablesLocal(rightNames, opts.RightVariables);
  end
  leftRowKeys = rowKeysLocal(leftSt, height(left), leftKeys);
  rightRowKeys = rowKeysLocal(rightSt, height(right), rightKeys);
  pairs = [];
  matchedRight = false(height(right), 1);
  for i = 1:height(left)
    matches = find(strcmp(rightRowKeys, leftRowKeys{i}));
    if isempty(matches)
      if any(strcmp(joinType, {'left', 'full'}))
        pairs = [pairs; i 0];
      end
    else
      for j = matches(:)'
        pairs = [pairs; i j];
        matchedRight(j) = true;
      end
    end
  end
  if any(strcmp(joinType, {'right', 'full'}))
    for j = find(~matchedRight(:))'
      pairs = [pairs; 0 j];
    end
  end
  columns = {};
  if opts.MergeKeys
    for k = 1:length(leftKeys)
      columns{end + 1} = struct('Source', 'key', 'LeftIndex', leftKeys(k), 'RightIndex', rightKeys(k), 'Name', leftKeyNames{k});
    end
    leftVariables = setdiffIndicesLocal(leftVariables, leftKeys);
    rightVariables = setdiffIndicesLocal(rightVariables, rightKeys);
  end
  for k = leftVariables(:)'
    columns{end + 1} = struct('Source', 'left', 'LeftIndex', k, 'RightIndex', 0, 'Name', leftNames{k});
  end
  for k = rightVariables(:)'
    columns{end + 1} = struct('Source', 'right', 'LeftIndex', 0, 'RightIndex', k, 'Name', rightNames{k});
  end
  resultNames = cell(1, length(columns));
  for k = 1:length(columns)
    resultNames{k} = columns{k}.Name;
  end
  resultNames = resolveJoinVariableNames(resultNames, columns);
  values = cell(1, length(resultNames));
  for k = 1:length(resultNames)
    values{k} = [];
  end
  for p = 1:size(pairs, 1)
    i = pairs(p, 1);
    j = pairs(p, 2);
    for outCol = 1:length(columns)
      column = columns{outCol};
      switch column.Source
        case 'key'
          if i ~= 0
            rowValue = columnRow(left, column.LeftIndex, i);
          else
            rowValue = columnRow(right, column.RightIndex, j);
          end
        case 'left'
          if i ~= 0
            rowValue = columnRow(left, column.LeftIndex, i);
          else
            rowValue = defaultValueLocal(columnSample(left, column.LeftIndex), 1);
          end
        case 'right'
          if j ~= 0
            rowValue = columnRow(right, column.RightIndex, j);
          else
            rowValue = defaultValueLocal(columnSample(right, column.RightIndex), 1);
          end
      end
      values{outCol} = appendValue(values{outCol}, rowValue, p);
    end
  end
  T = table(values{:}, 'VariableNames', resultNames);
end
%=============================================================================
function opts = parseJoinOptions(left, right, joinType, args)
  leftNames = left.Properties.VariableNames;
  rightNames = right.Properties.VariableNames;
  common = commonNames(leftNames, rightNames);
  if isempty(common)
    error(_('No common key variables found.'));
  end
  opts = struct();
  opts.LeftKeys = common;
  opts.RightKeys = common;
  opts.LeftVariables = [];
  opts.RightVariables = [];
  hasVariableSelection = false;
  opts.MergeKeys = ~strcmp(joinType, 'full') && ~strcmp(joinType, 'left') && ~strcmp(joinType, 'right');
  k = 1;
  while k <= length(args)
    name = args{k};
    if isstring(name)
      name = char(name);
    end
    switch lower(name)
      case 'keys'
        opts.LeftKeys = args{k + 1};
        opts.RightKeys = args{k + 1};
      case 'leftkeys'
        opts.LeftKeys = args{k + 1};
      case 'rightkeys'
        opts.RightKeys = args{k + 1};
      case 'leftvariables'
        opts.LeftVariables = args{k + 1};
        hasVariableSelection = true;
      case 'rightvariables'
        opts.RightVariables = args{k + 1};
        hasVariableSelection = true;
      case 'mergekeys'
        opts.MergeKeys = logical(args{k + 1});
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  if hasVariableSelection && strcmp(joinType, 'inner')
    opts.MergeKeys = false;
  end
end
%=============================================================================
function names = commonNames(leftNames, rightNames)
  names = {};
  for k = 1:length(leftNames)
    if any(strcmp(rightNames, leftNames{k}))
      names{end + 1} = leftNames{k};
    end
  end
end
%=============================================================================
function names = resolveJoinVariableNames(names, columns)
  names = makeValidNamesLocal(names);
  originalNames = names;
  for k = 1:length(names)
    if sum(strcmp(originalNames, originalNames{k})) > 1
      if strcmp(columns{k}.Source, 'left')
        names{k} = [names{k}, '_L'];
      elseif strcmp(columns{k}.Source, 'right')
        names{k} = [names{k}, '_R'];
      else
        names{k} = [names{k}, '_key'];
      end
    end
  end
  used = {};
  for k = 1:length(names)
    candidate = names{k};
    base = candidate;
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
function sample = columnSample(T, idx)
  st = struct(T);
  name = st.Properties.VariableNames{idx};
  sample = st.data.(name);
end
%=============================================================================
function rowValue = columnRow(T, idx, row)
  rowValue = columnRowsLocal(columnSample(T, idx), row);
end
%=============================================================================
function value = appendValue(value, rowValue, rowIndex)
  if rowIndex == 1 || isempty(value)
    value = rowValue;
  else
    try
      value = [value; rowValue];
    catch
      value = [toCellColumn(value); toCellColumn(rowValue)];
    end
  end
end
%=============================================================================
function value = toCellColumn(value)
  if iscell(value)
    value = value(:);
  else
    value = num2cell(value, 2);
  end
end
%=============================================================================
function idx = resolveVariablesLocal(names, vars)
  if nargin < 2 || isempty(vars) || (ischar(vars) && strcmp(vars, ':'))
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
function names = makeValidNamesLocal(names)
  if ischar(names)
    names = {names};
  elseif isstring(names)
    names = cellstr(names);
  end
  names = names(:)';
  for k = 1:length(names)
    name = names{k};
    if isempty(name)
      name = ['Var', num2str(k)];
    end
    name = regexprep(name, '[^a-zA-Z0-9_]', '_');
    if isempty(regexp(name(1), '[a-zA-Z]', 'once'))
      name = ['Var', name];
    end
    names{k} = name;
  end
end
%=============================================================================
