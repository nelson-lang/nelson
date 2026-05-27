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
  idx = tableResolveVariables(names, vars);
  newName = 'Merged';
  mergeAsTable = false;
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case 'newvariablename'
        newName = char(varargin{k + 1});
      case 'mergeastable'
        mergeAsTable = logical(varargin{k + 1});
      otherwise
        error(_('Unknown option.'));
    end
    k = k + 2;
  end
  if mergeAsTable
    merged = T(:, names(idx));
  else
    merged = T{:, names(idx)};
  end
  first = min(idx);
  T(:, names(idx)) = [];
  if width(T) == 0 || first > width(T)
    T = addvars(T, merged, 'NewVariableNames', {newName});
  else
    T = addvars(T, merged, 'NewVariableNames', {newName}, 'Before', first);
  end
end
%=============================================================================
