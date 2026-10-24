%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [TT, index] = sortrows(TT, vars, direction)
  narginchk(1, 3);
  st = struct(TT);
  if nargin < 2 || isempty(vars)
    [~, index] = sort(st.Properties.RowTimes);
  elseif ischar(vars) && strcmp(vars, st.Properties.DimensionNames{1})
    [~, index] = sort(st.Properties.RowTimes);
  elseif isstring(vars) && isscalar(vars) && strcmp(char(vars), st.Properties.DimensionNames{1})
    [~, index] = sort(st.Properties.RowTimes);
  else
    tableObject = table.fromStruct(st.Table);
    if nargin < 3
      [~, index] = sortrows(tableObject, vars);
    else
      [~, index] = sortrows(tableObject, vars, direction);
    end
  end
  if nargin >= 3 && ischar(direction) && strcmpi(direction, 'descend') && (nargin < 2 || isempty(vars) || strcmp(char(vars), st.Properties.DimensionNames{1}))
    index = flipud(index(:));
  end
  TT = TT(index, :);
end
%=============================================================================
