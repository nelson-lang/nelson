%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = tableFirstVariableRowNames(st, nRows)
  % Row names implied by the first variable when it holds text: a table
  % without RowNames can be indexed by these names.
  names = {};
  if isempty(st.Properties.VariableNames)
    return
  end
  value = st.data.(st.Properties.VariableNames{1});
  if isstring(value) && isvector(value)
    names = cellstr(value(:))';
  elseif iscellstr(value)
    names = value(:)';
  elseif ischar(value) && size(value, 1) == nRows
    names = cellstr(value)';
  end
end
%=============================================================================
