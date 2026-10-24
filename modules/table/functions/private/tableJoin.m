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
        error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), leftVars{lk});
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
        error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), rightVars{rk});
      end
      rightKeys(end + 1) = found;
    end
  end
  if length(leftKeys) ~= length(rightKeys)
    error('Nelson:table:dimagree:keyListsLength', 'Key lists must have the same length.');
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
  leftData = leftSt.data;
  rightData = rightSt.data;
  leftRowKeys = rowKeysLocal(leftSt, height(left), leftKeys);
  rightRowKeys = rowKeysLocal(rightSt, height(right), rightKeys);
  [leftIdx, rightIdx] = buildJoinPairs(leftRowKeys, rightRowKeys, height(left), height(right), joinType);
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
  for outCol = 1:length(columns)
    column = columns{outCol};
    switch column.Source
      case 'key'
        leftColumn = leftData.(leftNames{column.LeftIndex});
        rightColumn = rightData.(rightNames{column.RightIndex});
        values{outCol} = gatherKeyColumn(leftColumn, rightColumn, leftIdx, rightIdx);
      case 'left'
        leftColumn = leftData.(leftNames{column.LeftIndex});
        values{outCol} = gatherColumn(leftColumn, leftIdx);
      case 'right'
        rightColumn = rightData.(rightNames{column.RightIndex});
        values{outCol} = gatherColumn(rightColumn, rightIdx);
    end
  end
  T = table(values{:}, 'VariableNames', resultNames);
end
%=============================================================================
function [leftIdx, rightIdx] = buildJoinPairs(leftRowKeys, rightRowKeys, nLeft, nRight, joinType)
  % Group the right rows by key value once, then match each left row with a
  % hash lookup instead of scanning every right key. The pair order mirrors the
  % row-by-row loop it replaces: left rows ascending, each with its right
  % matches ascending, followed by any right-only rows ascending at the end.
  [uniqueRightKeys, ~, rightGroupOfRow] = unique(rightRowKeys);
  if nRight == 0
    rightRowsByGroup = {};
  else
    rightRowsByGroup = accumarray(rightGroupOfRow(:), (1:nRight)', [], @(x) {x});
  end
  [leftHasMatch, leftGroup] = ismember(leftRowKeys, uniqueRightKeys);
  includeLeftUnmatched = any(strcmp(joinType, {'left', 'full'}));
  matchedRight = false(nRight, 1);
  leftPairParts = cell(nLeft, 1);
  rightPairParts = cell(nLeft, 1);
  for i = 1:nLeft
    if leftHasMatch(i)
      matches = rightRowsByGroup{leftGroup(i)};
      leftPairParts{i} = repmat(i, numel(matches), 1);
      rightPairParts{i} = matches;
      matchedRight(matches) = true;
    elseif includeLeftUnmatched
      leftPairParts{i} = i;
      rightPairParts{i} = 0;
    end
  end
  leftIdx = vertcat(leftPairParts{:});
  rightIdx = vertcat(rightPairParts{:});
  leftIdx = leftIdx(:);
  rightIdx = rightIdx(:);
  if any(strcmp(joinType, {'right', 'full'}))
    unmatched = find(~matchedRight(:));
    leftIdx = [leftIdx; zeros(numel(unmatched), 1)];
    rightIdx = [rightIdx; unmatched];
  end
end
%=============================================================================
function out = gatherColumn(column, idx)
  if size(column, 1) == 0
    out = defaultValueLocal(column, numel(idx));
    return
  end
  valid = idx ~= 0;
  if all(valid)
    out = tableColumnRows(column, idx);
    return
  end
  safeIdx = idx;
  safeIdx(~valid) = 1;
  out = tableColumnRows(column, safeIdx);
  out = setColumnRows(out, ~valid, defaultValueLocal(column, sum(~valid)));
end
%=============================================================================
function out = gatherKeyColumn(leftColumn, rightColumn, leftIdx, rightIdx)
  fromLeft = leftIdx ~= 0;
  if all(fromLeft)
    out = tableColumnRows(leftColumn, leftIdx);
    return
  end
  if ~any(fromLeft)
    out = tableColumnRows(rightColumn, rightIdx);
    return
  end
  safeLeft = leftIdx;
  safeLeft(~fromLeft) = 1;
  out = tableColumnRows(leftColumn, safeLeft);
  fillValues = tableColumnRows(rightColumn, rightIdx(~fromLeft));
  out = setColumnRows(out, ~fromLeft, fillValues);
end
%=============================================================================
function value = setColumnRows(value, rowMask, newRows)
  if isvector(value)
    value(rowMask, :) = newRows;
  else
    idx = repmat({':'}, 1, ndims(value) - 1);
    value(rowMask, idx{:}) = newRows;
  end
end
%=============================================================================
function opts = parseJoinOptions(left, right, joinType, args)
  leftNames = left.Properties.VariableNames;
  rightNames = right.Properties.VariableNames;
  common = commonNames(leftNames, rightNames);
  if isempty(common)
    error('Nelson:table:noCommonKeys', 'No common key variables found.');
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
        error('Nelson:table:unknownOption', 'Unknown option.');
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
        error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), vars{k});
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
  % The row keys are built column by column with arrayfun and accumulated in a
  % string array. Building them with a row-by-row loop that stores each key into
  % a cell is quadratic in the interpreter (a char result from class() stored
  % per iteration triggers repeated reallocation); letting the builtin assemble
  % the column of keys keeps it linear. The resulting strings are textually
  % identical to the former char keys, so unique/ismember group rows the same.
  keys = strings(nRows, 1);
  nVars = length(vars);
  for j = 1:nVars
    name = st.Properties.VariableNames{vars(j)};
    col = st.data.(name);
    if nRows == 0
      part = strings(0, 1);
    else
      part = string(arrayfun(@(r) valueKeyLocal(tableColumnRows(col, r)), (1:nRows)', 'UniformOutput', false));
    end
    keys = keys + '#' + int2str(j) + '=' + part + ';';
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
