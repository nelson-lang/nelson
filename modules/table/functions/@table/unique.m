%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = unique(A, varargin)
  stA = struct(A);
  keys = rowKeysLocal(stA, height(A), 1:length(stA.Properties.VariableNames));
  [~, ia, ic] = uniqueStableLocal(keys);
  C = A(ia, :);
  values = {C, ia, ic};
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
