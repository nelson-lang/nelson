%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [st, rows] = tableBraceNewVariables(st, names, rows, nRows, value, rowSub)
  % T{rows, names} = value: the names that are not variables yet are
  % appended, typed after value (one column of value per name when several
  % names are given). The caller then assigns the selected rows.
  isNew = ~ismember(names, st.Properties.VariableNames);
  if ~any(isNew)
    return
  end
  samples = cell(1, length(names));
  for k = find(isNew)
    if length(names) == 1
      samples{k} = value;
    else
      samples{k} = value(:, k);
    end
  end
  [st, rows] = tableAppendVariables(st, names, isNew, rows, nRows, samples, rowSub);
end
%=============================================================================
