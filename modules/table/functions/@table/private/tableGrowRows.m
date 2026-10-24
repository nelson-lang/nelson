%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function st = tableGrowRows(st, nRows, assignedNames, newRowNames)
  % After an assignment to rows past the height nRows of the table: the
  % variables that were not assigned grow to the new height with their
  % default value (with a warning when they existed before), and the new
  % rows get names when the table has row names: newRowNames, the names
  % given in the row subscript, then Row<k>.
  names = st.Properties.VariableNames;
  heights = zeros(1, length(names));
  for k = 1:length(names)
    heights(k) = size(st.data.(names{k}), 1);
  end
  newHeight = max([heights, nRows + length(newRowNames)]);
  if newHeight <= nRows
    return
  end
  isShort = heights < newHeight;
  for k = find(isShort)
    st.data.(names{k}) = tableGrowColumn(st.data.(names{k}), newHeight);
  end
  if any(isShort & ~ismember(names, assignedNames))
    warning('Nelson:table:rowsAddedExistingVars', _('The assignment added rows to the table, but did not assign values to all of the table''s existing variables. Those variables are extended with rows containing default values.'));
  end
  rowNames = st.Properties.RowNames;
  if isempty(rowNames) && isempty(newRowNames)
    return
  end
  rowNames = rowNames(:);
  newRowNames = newRowNames(:);
  first = tableDefaultRowNames(length(rowNames) + 1:nRows, [rowNames; newRowNames]);
  rowNames = [rowNames; first; newRowNames];
  last = tableDefaultRowNames(length(rowNames) + 1:newHeight, rowNames);
  st.Properties.RowNames = [rowNames; last];
end
%=============================================================================
function column = tableGrowColumn(column, nRows)
  % The rows added fill in as growing an array of that class does: 0,
  % false, {[]}, <missing>, <undefined>, NaT, 0 sec.
  idx = repmat({':'}, 1, ndims(column) - 1);
  if size(column, 1) > 0
    column(nRows + 1, idx{:}) = column(1, idx{:});
    column = column(1:nRows, idx{:});
  elseif isa(column, 'duration')
    column(nRows, idx{:}) = seconds(0);
  elseif iscell(column)
    column(nRows, idx{:}) = {[]};
  elseif isnumeric(column) || islogical(column) || ischar(column)
    column(nRows, idx{:}) = cast(0, class(column));
  else
    column(nRows, idx{:}) = missing;
  end
end
%=============================================================================
