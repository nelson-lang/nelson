%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T2 = rows2vars(T, varargin)
  narginchk(1, Inf);
  mustBeA(T, 'table', 1);
  variableNamesSource = '';
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case 'variablenamessource'
        variableNamesSource = char(varargin{k + 1});
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  names = T.Properties.VariableNames;
  if isempty(variableNamesSource)
    newNames = cell(1, height(T));
    for r = 1:height(T)
      newNames{r} = ['Var', num2str(r)];
    end
  else
    source = T.(variableNamesSource);
    if isstring(source)
      newNames = cellstr(source);
    elseif iscellstr(source)
      newNames = source(:)';
    else
      error(_('VariableNamesSource must contain text.'));
    end
  end
  data = cell(length(names), height(T) + 1);
  for j = 1:length(names)
    data{j, 1} = names{j};
    column = T.(names{j});
    for r = 1:height(T)
      value = tableColumnRows(column, r);
      if iscell(value)
        value = value{1};
      end
      data{j, r + 1} = value;
    end
  end
  T2 = cell2table(data, 'VariableNames', ['OriginalVariableNames', tableMakeUniqueNames(newNames)]);
end
%=============================================================================
