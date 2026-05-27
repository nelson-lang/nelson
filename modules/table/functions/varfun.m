%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = varfun(fun, T, varargin)
  narginchk(2, Inf);
  mustBeA(T, 'table', 2);
  vars = 1:width(T);
  outputFormat = 'table';
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
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  values = cell(1, length(vars));
  outNames = cell(1, length(vars));
  for j = 1:length(vars)
    name = names{vars(j)};
    values{j} = fun(T.(name));
    outNames{j} = [func2str(fun), '_', name];
  end
  switch outputFormat
    case 'table'
      R = table(values{:}, 'VariableNames', tableMakeUniqueNames(outNames));
    case 'uniform'
      R = [values{:}];
    case 'cell'
      R = values;
    otherwise
      error(_('Unsupported OutputFormat.'));
  end
end
%=============================================================================
