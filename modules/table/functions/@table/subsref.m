%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = subsref(T, sref)
  % Get types of subsref being used, e.g., '.', '{}', or '()'
  TYPES = {sref.type};
  % Determine which type of subscript is being used and delegate to helper function
  switch TYPES{1}
    case '.'
      R = dotSubsref(T, sref);
      
    case '{}'
      R = braceSubsref(T, sref);
      
    case '()'
      R = parentheseSubsref(T, sref);
      
    otherwise
      error(_('Unsupported subsref type.'));
    end
  end
  %=============================================================================
function R = dotSubsref(T, sref)
  st = struct(T);
  if any(contains(st.Properties.VariableNames, 'Row'))
    rowPropertyName = 'Row_1';
  else 
    rowPropertyName = 'Row';
  end
  if ischar(sref(1).subs) && strcmp(sref(1).subs, rowPropertyName)
    R = st.Properties.RowNames;
    if isrow(R)
      R = R';
    end
    if length(sref) > 1
      R = subsref(R, sref(2:end));
    end
    return;
  end
  if ischar(sref(1).subs) && strcmp(sref(1).subs, 'Variables')
    R = T{:, :};
    if length(sref) > 1
      R = subsref(R, sref(2:end));
    end
    return;
  end
  if ischar(sref(1).subs) && strcmp(sref(1).subs, 'Properties')
    R = st.Properties;
    if length(sref) > 1
      R = subsref(R, sref(2:end));
    end
    return;
  end
  tmp = st.data;
  R = tmp.(sref(1).subs);
  TYPES = {sref.type};
  if length(TYPES) > 1
    R = subsref(R, sref(2:end));
  end
end
%=============================================================================
function R = braceSubsref(T, sref)
  
  if (length(sref(1).subs) ~= 2)
    error('Unsupported subscript type. Brace indexing requires two subscripts (row and column).');
  end
  
  st = struct(T);
  
  rowIdx = sref(1).subs{1};
  colSub = sref(1).subs{2};
  
  if ischar(rowIdx) || isstring(rowIdx) || iscellstr(rowIdx)
    if (isscalar(rowIdx) && rowIdx == ":")
      rowIdx = 1:height(T);
    else
      rowIdx = find(ismember(st.Properties.RowNames, rowIdx));
      if any(rowIdx == 0)
        error(_('One or more row names not found.'));
      end
    end
  elseif (isnumeric(rowIdx) || islogical(rowIdx))
    rowIdx = rowIdx(:);
  else
    error(_('Invalid row subscript type.'));
  end
  
  if (ischar(colSub) || isstring(colSub) || iscellstr(colSub))
    if (isscalar(colSub) && colSub == ":")
      colSub = 1:width(T);
    else
      colSub = find(contains(st.Properties.VariableNames, colSub) == true);
    end
  elseif (isnumeric(colSub) || islogical(colSub))
    colSub = colSub(:);
  else
    error(_('Invalid column subscript type.'));
  end
  numRows = length(rowIdx);
  numCols = length(colSub);
  R = [];
  
  for i = 1:numCols
    variable = st.Properties.VariableNames{colSub(i)};
    tempdata = st.data.(variable);
    if isnumeric(tempdata) || islogical(tempdata) || isstring(tempdata) || ischar(tempdata)
      nbIdx = ndims(tempdata) - 1;
      idx = repmat({':'}, 1, nbIdx);
      R = [R, tempdata(rowIdx, idx{:})];
    else
      R(:, i) = tempdata(rowIdx);
    end
  end
end
%=============================================================================
function R = parentheseSubsref(T, sref)
  st = struct(T);
  
  % Get the subscripts
  subs = sref(1).subs;
  
  % Check if we have one or two subscripts
  if length(subs) == 1
    error(_('Invalid number of subscripts.'));
  elseif length(subs) == 2
    % Row and column indexing
    rowIdx = subs{1};
    colIdx = subs{2};
  else
    error(_('Invalid number of subscripts.'));
  end
  
  % Handle row indexing
  if ischar(rowIdx) && strcmp(rowIdx, ':')
    rowIdx = 1:height(T);
  elseif islogical(rowIdx)
    rowIdx = find(rowIdx);
  elseif isnumeric(rowIdx)
    % Keep as is
  elseif ischar(rowIdx) || iscellstr(rowIdx) || isstring(rowIdx)
    % Handle row names
    if ischar(rowIdx)
      rowIdx = {rowIdx};
    end
    rowIdx = find(ismember(st.Properties.RowNames, rowIdx));
    if isempty(rowIdx)
      error(_('Row name not found.'));
    end
  else
    error(_('Invalid row indexing.'));
  end
  
  % Handle column indexing
  if ischar(colIdx) && strcmp(colIdx, ':')
    colIdx = 1:width(T);
  elseif ischar(colIdx) && isrow(colIdx)
    colIdx = find(ismember(st.Properties.VariableNames, {colIdx}));
  elseif islogical(colIdx)
    colIdx = find(colIdx);
  elseif isnumeric(colIdx)
    % Keep as is
  elseif iscellstr(colIdx) || isstring(colIdx)
    colIdx = find(ismember(st.Properties.VariableNames, colIdx));
  else
    error(_('Invalid column indexing.'));
  end
  
  selectedVarNames = st.Properties.VariableNames(colIdx);
  tableData = cell(1, length(selectedVarNames));
  for i = 1:length(selectedVarNames)
    V = st.data.(selectedVarNames{i});
    tableData{i} = V(rowIdx, :);
  end
  
  % Set row names if they exist in the original table
  if ~isempty(st.Properties.RowNames)
    RowNames = st.Properties.RowNames(rowIdx);
    R = table(tableData{:}, 'VariableNames', selectedVarNames, 'RowNames', RowNames);
  else
    R = table(tableData{:}, 'VariableNames', selectedVarNames);
  end
  
  % Handle further subscripting if necessary
  if length(sref) > 1
    R = subsref(R, sref(2:end));
  end
end
%=============================================================================
