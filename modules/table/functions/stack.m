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
  stackedName = '';
  indexName = '';
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case {'newdataname', 'newdatavariablename'}
        stackedName = char(varargin{k + 1});
      case 'indexvariablename'
        indexName = char(varargin{k + 1});
      otherwise
        error('Nelson:table:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  if isempty(stackedName)
    stackedName = strjoin(names(idx), '_');
  end
  if isempty(indexName)
    indexName = [stackedName, '_Indicator'];
  end
  keep = setdiffIndicesLocal(1:width(T), idx);
  keptNames = names(keep);
  stackedNames = names(idx);
  nStack = numel(idx);
  h = height(T);
  % Each original row produces nStack output rows (one per stacked variable),
  % the stacked variables cycling inside each original row. Build the columns
  % in one vectorised pass instead of growing the table row by row.
  rowSelector = repelem((1:h)', nStack);
  out = table();
  for k = 1:numel(keep)
    out.(keptNames{k}) = columnRowsLocal(st.data.(keptNames{k}), rowSelector);
  end
  out.(indexName) = repmat(stackedNames(:), h, 1);
  out.(stackedName) = stackValuesLocal(st.data, stackedNames, h, nStack);
  S = out;
end
%=============================================================================
function values = stackValuesLocal(data, stackedNames, h, nStack)
  total = h * nStack;
  template = data.(stackedNames{1});
  columns = cell(1, nStack);
  for p = 1:nStack
    columns{p} = data.(stackedNames{p});
  end
  if iscell(template)
    values = cell(total, size(template, 2));
    for p = 1:nStack
      values(p:nStack:total, :) = columns{p};
    end
  else
    % Interleave the stacked columns so that, for each original row, the
    % values cycle through the stacked variables (row-major ordering).
    stacked3d = cat(3, columns{:});
    stacked3d = permute(stacked3d, [3, 1, 2]);
    values = reshape(stacked3d, total, []);
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
function value = columnRowsLocal(value, rows)
  if isvector(value)
    value = value(rows, :);
  else
    idx = repmat({':'}, 1, ndims(value) - 1);
    value = value(rows, idx{:});
  end
end
%=============================================================================
