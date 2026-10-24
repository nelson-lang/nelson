%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = array2table(varargin)
  narginchk(1, inf);
  nargoutchk(0, 1);
  X = varargin{1};
  if ~ismatrix(X)
    error('Nelson:table:invalidType:xA2DMatrix', 'X must be a 2-D matrix.');
  end
  if mod(nargin - 1, 2) ~= 0
    error('Nelson:table:nameValuePair:nameValueArgumentsComePairs', 'Name-value arguments must come in pairs.');
  end
  varNames = {};
  rowNames = {};
  dimNames = {};
  hasVarNames = false;
  for k = 2:2:nargin
    name = varargin{k};
    if ~(ischar(name) && isrow(name)) && ~(isstring(name) && isscalar(name))
      error('Nelson:table:invalidType:parameterNameACharacterVectorString', 'Parameter name must be a character vector or string scalar.');
    end
    value = varargin{k + 1};
    switch lower(char(name))
      case 'variablenames'
        varNames = value;
        hasVarNames = true;
      case 'rownames'
        rowNames = value;
      case 'dimensionnames'
        dimNames = value;
      otherwise
        error('Nelson:table:unknownParameter', 'Unknown parameter name.');
    end
  end
  nCols = size(X, 2);
  values = cell(1, nCols);
  for iCol = 1:nCols
    values{iCol} = X(:, iCol);
  end
  if ~hasVarNames
    prefix = inputname(1);
    if isempty(prefix)
      prefix = 'Var';
    end
    varNames = cell(1, nCols);
    for iCol = 1:nCols
      varNames{iCol} = sprintf('%s%d', prefix, iCol);
    end
  end
  args = [values, {'VariableNames', varNames}];
  if nCols == 0
    args = [args, {'Size', [size(X, 1), 0], 'VariableTypes', {}}];
  end
  if ~isempty(rowNames)
    args = [args, {'RowNames', rowNames}];
  end
  if ~isempty(dimNames)
    args = [args, {'DimensionNames', dimNames}];
  end
  varargout{1} = table(args{:});
end
%=============================================================================
