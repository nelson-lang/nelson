%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function S = stack(T, vars, varargin)
  narginchk(2, Inf);
  mustBeA(T, 'table', 1);
  st = struct(T);
  names = st.Properties.VariableNames;
  idx = resolveVariablesLocal(names, vars);
  stackedName = 'StackedData';
  indexName = 'Indicator';
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case {'newdataname', 'newdatavariablename'}
        stackedName = char(varargin{k + 1});
      case 'indexvariablename'
        indexName = char(varargin{k + 1});
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  keep = setdiffIndicesLocal(1:width(T), idx);
  out = table();
  values = {};
  indicators = {};
  for r = 1:height(T)
    for j = idx
      row = T(r, names(keep));
      row.(indexName) = {names{j}};
      value = columnRowsLocal(st.data.(names{j}), r);
      if iscell(value)
        value = value{1};
      end
      row.(stackedName) = value;
      if isempty(out)
        out = row;
      else
        out = [out; row];
      end
    end
  end
  S = out;
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
function value = columnRowsLocal(value, rows)
  if isvector(value)
    value = value(rows, :);
  else
    idx = repmat({':'}, 1, ndims(value) - 1);
    value = value(rows, idx{:});
  end
end
%=============================================================================
