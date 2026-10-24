%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [st, rows] = tableParenNewVariables(st, names, rows, nRows, valueSt, rowSub)
  % T(rows, names) = tableValue: the names that are not variables yet are
  % appended, typed after the variables of tableValue, in order. The caller
  % then assigns the selected rows.
  isNew = ~ismember(names, st.Properties.VariableNames);
  if ~any(isNew)
    return
  end
  sourceNames = valueSt.Properties.VariableNames;
  samples = cell(1, length(names));
  for k = find(isNew)
    samples{k} = valueSt.data.(sourceNames{k});
  end
  [st, rows] = tableAppendVariables(st, names, isNew, rows, nRows, samples, rowSub);
end
%=============================================================================
