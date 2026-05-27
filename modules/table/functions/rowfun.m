%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = rowfun(fun, T, varargin)
  narginchk(2, Inf);
  mustBeA(T, 'table', 2);
  vars = 1:width(T);
  outputFormat = 'table';
  outputVariableNames = {'Fun'};
  st = struct(T);
  names = st.Properties.VariableNames;
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case 'inputvariables'
        vars = tableResolveVariables(names, varargin{k + 1}, st.Properties.VariableTypes, st.data);
      case 'outputformat'
        outputFormat = lower(char(varargin{k + 1}));
      case 'outputvariablenames'
        outputVariableNames = varargin{k + 1};
        if ischar(outputVariableNames)
          outputVariableNames = {outputVariableNames};
        elseif isstring(outputVariableNames)
          outputVariableNames = cellstr(outputVariableNames);
        end
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  values = cell(height(T), 1);
  for r = 1:height(T)
    args = cell(1, length(vars));
    for j = 1:length(vars)
      args{j} = tableColumnRows(T.(names{vars(j)}), r);
    end
    values{r} = fun(args{:});
  end
  switch outputFormat
    case 'table'
      try
        R = table(cell2mat(values), 'VariableNames', outputVariableNames(1));
      catch
        R = table(values, 'VariableNames', outputVariableNames(1));
      end
    case 'uniform'
      R = cell2mat(values);
    case 'cell'
      R = values;
    otherwise
      error(_('Unsupported OutputFormat.'));
  end
end
%=============================================================================
