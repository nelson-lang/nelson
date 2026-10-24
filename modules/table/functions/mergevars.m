%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = mergevars(T, vars, varargin)
  narginchk(2, Inf);
  mustBeA(T, 'table', 1);
  names = T.Properties.VariableNames;
  newName = '';
  mergeAsTable = false;
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case 'newvariablename'
        newName = varargin{k + 1};
        if ~((ischar(newName) && isrow(newName)) || (isstring(newName) && isscalar(newName)))
          error('Nelson:table:mergevars:InvalidNewVarName', 'Specify the new variable name as a string or a character vector.');
        end
        newName = char(newName);
      case 'mergeastable'
        mergeAsTable = logical(varargin{k + 1});
      otherwise
        error('Nelson:table:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  if isempty(vars) && ~ischar(vars)
    return
  end
  idx = tableResolveVariables(names, vars);
  if mergeAsTable
    merged = T(:, idx);
  else
    merged = T{:, idx};
  end
  % The merged variable takes the place of the first variable listed.
  position = idx(1) - nnz(unique(idx) < idx(1));
  T(:, unique(idx)) = [];
  if isempty(newName)
    newName = nelson.lang.makeUniqueStrings(sprintf('Var%d', position), T.Properties.VariableNames);
  end
  if position > width(T)
    T = addvars(T, merged, 'NewVariableNames', {newName});
  else
    T = addvars(T, merged, 'NewVariableNames', {newName}, 'Before', position);
  end
end
%=============================================================================
