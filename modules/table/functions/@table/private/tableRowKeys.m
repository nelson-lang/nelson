%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function keys = tableRowKeys(st, nRows, vars)
  if nargin < 3 || isempty(vars)
    vars = 1:length(st.Properties.VariableNames);
  end
  keys = cell(nRows, 1);
  for r = 1:nRows
    key = '';
    for j = 1:length(vars)
      name = st.Properties.VariableNames{vars(j)};
      column = st.data.(name);
      key = [key, '#', int2str(j), '=', tableValueKey(tableColumnRows(column, r)), ';'];
    end
    keys{r} = key;
  end
end
%=============================================================================
