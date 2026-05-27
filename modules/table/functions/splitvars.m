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
  narginchk(2, Inf);
  mustBeA(T, 'table', 1);
  names = T.Properties.VariableNames;
  idx = tableResolveVariables(names, vars);
  if length(idx) ~= 1
    error(_('One variable expected.'));
  end
  name = names{idx};
  value = T.(name);
  newNames = {};
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case 'newvariablenames'
        newNames = varargin{k + 1};
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  if istable(value)
    addNames = value.Properties.VariableNames;
    addValues = cell(1, length(addNames));
    for j = 1:length(addNames)
      addValues{j} = value.(addNames{j});
    end
  elseif iscell(value) && iscolumn(value)
    widthValue = size(value{1}, 2);
    addValues = cell(1, widthValue);
    for j = 1:widthValue
      column = cell(height(T), 1);
      for r = 1:height(T)
        column{r} = value{r}(:, j);
      end
      addValues{j} = column;
    end
    addNames = defaultSplitNames(name, widthValue);
  else
    widthValue = size(value, 2);
    addValues = cell(1, widthValue);
    for j = 1:widthValue
      addValues{j} = value(:, j);
    end
    addNames = defaultSplitNames(name, widthValue);
  end
  if ~isempty(newNames)
    if isstring(newNames)
      newNames = cellstr(newNames);
    elseif ischar(newNames)
      newNames = {newNames};
    end
    addNames = newNames(:)';
  end
  T(:, name) = [];
  if idx > width(T)
    T = addvars(T, addValues{:}, 'NewVariableNames', addNames);
  else
    T = addvars(T, addValues{:}, 'NewVariableNames', addNames, 'Before', idx);
  end
end
%=============================================================================
function names = defaultSplitNames(base, n)
  names = cell(1, n);
  for k = 1:n
    names{k} = [base, '_', num2str(k)];
  end
end
%=============================================================================
