%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = splitvars(T, vars, varargin)
  narginchk(1, Inf);
  mustBeA(T, 'table', 1);
  st = struct(T);
  names = st.Properties.VariableNames;
  % Read every variable once by position. Looking up st.data.(names{k}) by
  % dynamic field name inside the per-variable loop is a linear field scan,
  % making splitvars O(numVars^2) on a wide table.
  allVals = struct2cell(st.data);
  if nargin < 2
    % Every variable with several columns or holding a table is split.
    idx = [];
    for k = 1:length(names)
      value = allVals{k};
      if istable(value) || columnCount(value) > 1
        idx(end + 1) = k;
      end
    end
    newNames = cell(1, length(idx));
  else
    [idx, newNames] = resolveSplitVariables(names, vars, varargin);
  end
  if isempty(idx)
    return
  end
  isSplit = false(1, length(names));
  isSplit(idx) = true;
  taken = names(~isSplit);
  outNames = {};
  outPieces = {};
  sourceParts = cell(1, length(names));
  for k = 1:length(names)
    if ~isSplit(k)
      pieceNames = names(k);
      pieces = {allVals{k}};
    else
      [pieces, pieceNames] = splitValue(allVals{k}, names{k}, taken);
      userNames = newNames{find(idx == k, 1)};
      if ~isempty(userNames)
        if length(userNames) ~= length(pieces)
          error('Nelson:table:dimagree:newVariableNamesMatchNumberAddedVariables', 'NewVariableNames must match the number of added variables.');
        end
        pieceNames = userNames;
      end
      taken = [taken, pieceNames];
    end
    for j = 1:length(pieces)
      outNames{end + 1} = pieceNames{j};
      outPieces{end + 1} = pieces{j};
    end
    sourceParts{k} = k * ones(1, length(pieces));
  end
  % Reject duplicate output names once (the per-piece scan was O(numVars^2))
  % and build the data struct in a single pass instead of adding one field at
  % a time, which reallocated the whole struct on each add.
  if length(unique(outNames)) ~= length(outNames)
    error('Nelson:table:duplicateVariableNames', 'Duplicate table variable name.');
  end
  sourceIndex = [sourceParts{:}];
  st.data = __tableStructFromCell__(outPieces(:), outNames(:));
  st.Properties.VariableNames = outNames;
  st.Properties.VariableTypes = string([]);
  st.Properties.VariableUnits = replicateMetadata(st.Properties.VariableUnits, sourceIndex);
  st.Properties.VariableDescriptions = replicateMetadata(st.Properties.VariableDescriptions, sourceIndex);
  st.Properties.VariableContinuity = replicateMetadata(st.Properties.VariableContinuity, sourceIndex);
  T = table.fromStruct(st);
end
%=============================================================================
function [idx, newNames] = resolveSplitVariables(names, vars, options)
  if strcmp(class(vars), 'vartype')
    error('Nelson:table:splitvars:vartypeInvalidVars', 'A vartype subscripter cannot specify the variables to split.');
  end
  userNames = {};
  k = 1;
  while k <= length(options)
    if k == length(options)
      error('Nelson:table:nameValuePair:nameValueArgumentFollowedByA', 'Name-value argument must be followed by a value.');
    end
    switch lower(char(options{k}))
      case 'newvariablenames'
        userNames = options{k + 1};
      otherwise
        error('Nelson:table:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  if isempty(vars)
    idx = [];
  else
    idx = tableResolveVariables(names, vars);
  end
  idx = idx(:)';
  if length(unique(idx)) ~= length(idx)
    error('Nelson:table:splitvars:duplicateVars', 'List of variables to split must not specify any variable more than once.');
  end
  newNames = cell(1, length(idx));
  if ~isempty(userNames)
    if length(idx) == 1
      newNames{1} = namesAsCellstr(userNames);
    else
      if ~iscell(userNames) || length(userNames) ~= length(idx)
        error('Nelson:table:splitvars:incorrectNumVarnamesMultisplit', 'When splitting several variables, NewVariableNames must be a cell array with one cell of names per variable.');
      end
      for k = 1:length(idx)
        if ~iscell(userNames{k}) && ~isstring(userNames{k})
          error('Nelson:table:splitvars:incorrectNumVarnamesMultisplit', 'When splitting several variables, NewVariableNames must be a cell array with one cell of names per variable.');
        end
        newNames{k} = namesAsCellstr(userNames{k});
      end
    end
  end
  [idx, order] = sort(idx);
  newNames = newNames(order);
end
%=============================================================================
function [pieces, pieceNames] = splitValue(value, name, taken)
  if istable(value)
    pieceNames = value.Properties.VariableNames;
    pieces = cell(1, length(pieceNames));
    for j = 1:length(pieceNames)
      pieces{j} = value.(pieceNames{j});
      if any(strcmp(taken, pieceNames{j}))
        pieceNames{j} = [name, '_', pieceNames{j}];
      end
    end
    return
  end
  nbColumns = columnCount(value);
  if nbColumns == 1
    pieces = {value};
    pieceNames = {name};
    return
  end
  value = reshape(value, size(value, 1), nbColumns);
  pieces = cell(1, nbColumns);
  pieceNames = cell(1, nbColumns);
  for j = 1:nbColumns
    pieces{j} = value(:, j);
    pieceNames{j} = uniqueName([name, '_', num2str(j)], taken);
  end
end
%=============================================================================
function n = columnCount(value)
  % An N-D variable splits along all its trailing dimensions.
  sz = size(value);
  n = prod(sz(2:end));
end
%=============================================================================
function name = uniqueName(name, taken)
  if ~any(strcmp(taken, name))
    return
  end
  k = 1;
  while any(strcmp(taken, [name, '_', num2str(k)]))
    k = k + 1;
  end
  name = [name, '_', num2str(k)];
end
%=============================================================================
function names = namesAsCellstr(names)
  if ischar(names)
    names = {names};
  elseif isstring(names)
    names = cellstr(names);
  end
  if ~iscellstr(names)
    error('Nelson:table:invalidVariableNames', 'New variable names must be a character vector, string array, or cell array of character vectors.');
  end
  names = names(:)';
end
%=============================================================================
function value = replicateMetadata(value, sourceIndex)
  if ~isempty(value)
    value = value(sourceIndex);
  end
end
%=============================================================================
