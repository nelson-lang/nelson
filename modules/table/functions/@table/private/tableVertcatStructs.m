%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [st, nRows] = tableVertcatStructs(st, nRows, stB, nB)
  % [A; B] on the structures of two tables of heights nRows and nB: a
  % table without rows adds nothing but its row names.
  rowNames = tableVertcatRowNames(st.Properties.RowNames, nRows, stB.Properties.RowNames, nB);
  if nRows == 0
    st = stB;
  elseif nB > 0
    names = st.Properties.VariableNames;
    if ~isequal(names, stB.Properties.VariableNames)
      error('Nelson:table:vertcatVariableNamesMismatch', 'All tables to be vertically concatenated must have identical variable names.');
    end
    if isempty(names)
      st.data = repmat(struct(), nRows + nB, 0);
    end
    for j = 1:length(names)
      st.data.(names{j}) = [st.data.(names{j}); stB.data.(names{j})];
    end
  end
  st.Properties.RowNames = rowNames;
  nRows = nRows + nB;
end
%=============================================================================
