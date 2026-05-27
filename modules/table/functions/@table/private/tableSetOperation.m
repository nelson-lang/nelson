%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tableSetOperation(op, A, B, varargin)
  validateCompatibleTables(A, B);
  stA = struct(A);
  stB = struct(B);
  keysA = rowKeysLocal(stA, height(A), 1:length(stA.Properties.VariableNames));
  keysB = rowKeysLocal(stB, height(B), 1:length(stB.Properties.VariableNames));
  switch op
    case 'intersect'
      [ia, ib] = intersectKeysLocal(keysA, keysB);
      C = A(ia, :);
      values = {C, ia, ib};
    case 'setdiff'
      ia = setdiffKeysLocal(keysA, keysB);
      C = A(ia, :);
      values = {C, ia};
    case 'union'
      combined = [A; B];
      stCombined = struct(combined);
      keys = rowKeysLocal(stCombined, height(combined), 1:length(stCombined.Properties.VariableNames));
      [~, selected] = uniqueStableLocal(keys);
      C = combined(selected, :);
      ia = selected(selected <= height(A));
      ib = selected(selected > height(A)) - height(A);
      values = {C, ia, ib};
    case 'setxor'
      ia = setdiffKeysLocal(keysA, keysB);
      ib = setdiffKeysLocal(keysB, keysA);
      C = [A(ia, :); B(ib, :)];
      values = {C, ia, ib};
    otherwise
      error(_('Unsupported table set operation.'));
  end
  varargout = values(1:nargout);
end
%=============================================================================
function validateCompatibleTables(A, B)
  if ~istable(B)
    error(_('Second input must be a table.'));
  end
  stA = struct(A);
  stB = struct(B);
  if ~isequal(stA.Properties.VariableNames, stB.Properties.VariableNames)
    error(_('Table variable names must match.'));
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
function [ia, ib] = intersectKeysLocal(keysA, keysB)
  ia = [];
  ib = [];
  used = {};
  for k = 1:length(keysA)
    found = find(strcmp(keysB, keysA{k}), 1);
    if ~isempty(found) && ~any(strcmp(used, keysA{k}))
      ia(end + 1) = k;
      ib(end + 1) = found;
      used{end + 1} = keysA{k};
    end
  end
  ia = ia(:);
  ib = ib(:);
end
%=============================================================================
function ia = setdiffKeysLocal(keysA, keysB)
  ia = [];
  used = {};
  for k = 1:length(keysA)
    if isempty(find(strcmp(keysB, keysA{k}), 1)) && ~any(strcmp(used, keysA{k}))
      ia(end + 1) = k;
      used{end + 1} = keysA{k};
    end
  end
  ia = ia(:);
end
%=============================================================================
