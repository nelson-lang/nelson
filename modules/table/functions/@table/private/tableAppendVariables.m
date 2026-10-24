%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [st, rows] = tableAppendVariables(st, names, isNew, rows, nRows, samples, rowSub)
  % Appends names(isNew) as columns of the default value of the class of
  % their sample (what growing an array of that class fills in), with the
  % width of the sample. The caller assigns the selected rows; the other
  % rows keep the default value, with a warning when the row subscript
  % rowSub is not given by row names.
  if isempty(st.Properties.VariableNames)
    st.data = struct();
    if nRows == 0
      nRows = size(samples{find(isNew, 1)}, 1);
      rows = 1:nRows;
    end
  end
  for k = find(isNew)
    sample = samples{k};
    nSample = size(sample, 1);
    if (nSample ~= numel(rows) && nSample ~= 1) || (nSample == 0 && nRows > 0)
      error('Nelson:table:dimagree:valueAssignmentNumberRowsTable', 'Value assignment must have the same number of rows as the table.');
    end
    st.data.(names{k}) = tableDefaultColumn(sample, nRows);
    st.Properties.VariableNames{end + 1} = names{k};
  end
  isRowNames = isstring(rowSub) || iscellstr(rowSub) || (ischar(rowSub) && ~strcmp(rowSub, ':'));
  if ~isRowNames && ~isempty(rows) && max(rows) <= nRows && numel(unique(rows)) < nRows
    warning('Nelson:table:rowsAddedNewVars', 'The new variables being added to the table have fewer rows than the table. They have been extended with rows containing default values.');
  end
end
%=============================================================================
