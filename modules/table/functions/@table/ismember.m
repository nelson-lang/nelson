%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ismember(A, B, varargin)
  if ~istable(B)
    error(_('Second input must be a table.'));
  end
  stA = struct(A);
  stB = struct(B);
  if ~isequal(stA.Properties.VariableNames, stB.Properties.VariableNames)
    error(_('Table variable names must match.'));
  end
  keysA = rowKeysLocal(stA, height(A), 1:length(stA.Properties.VariableNames));
  keysB = rowKeysLocal(stB, height(B), 1:length(stB.Properties.VariableNames));
  [tf, loc] = ismemberKeysLocal(keysA, keysB);
  values = {tf, loc};
  varargout = values(1:nargout);
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
function [tf, loc] = ismemberKeysLocal(keysA, keysB)
  tf = false(length(keysA), 1);
  loc = zeros(length(keysA), 1);
  for k = 1:length(keysA)
    found = find(strcmp(keysB, keysA{k}), 1);
    if ~isempty(found)
      tf(k) = true;
      loc(k) = found;
    end
  end
end
%=============================================================================
