%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function st = tableHorzcatStructs(st, stB, nRows)
  % [A, B] on the structures of two tables of nRows rows: the variables of
  % B are appended, its rows matched to the row names of A.
  [st.Properties.RowNames, rows] = tableHorzcatRowNames(st.Properties.RowNames, stB.Properties.RowNames, nRows);
  % Append every variable of B in one pass. Assigning st.data.(name) and
  % growing st.Properties.VariableNames per variable recopies the data struct
  % and the name list each time, making a wide horzcat O(numVars^2).
  bNames = stB.Properties.VariableNames;
  if isempty(bNames)
    return
  end
  if any(ismember(bNames, st.Properties.VariableNames))
    error('Nelson:table:duplicateVariableNames', 'Duplicate table variable name.');
  end
  bVals = struct2cell(stB.data);
  [~, bLoc] = ismember(bNames, fieldnames(stB.data));
  bVals = bVals(bLoc);
  if ~isempty(rows)
    for j = 1:numel(bVals)
      bVals{j} = tableColumnRows(bVals{j}, rows);
    end
  end
  st.data = __tableStructFromCell__([struct2cell(st.data); bVals(:)], [fieldnames(st.data); bNames(:)]);
  st.Properties.VariableNames = [st.Properties.VariableNames, bNames];
end
%=============================================================================
