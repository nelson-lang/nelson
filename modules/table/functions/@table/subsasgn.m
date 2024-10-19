%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = subsasgn(T, sasgn, value)
  TYPES = {sasgn.type};
  switch TYPES{1}
    case '.'
      R = dotSubsasgn(T, sasgn, value);
    case '{}'
      R = braceSubsasgn(T, sasgn, value);
    case '()'
      R = parentheseSubsasgn(T, sasgn, value);
      
    otherwise
      error(_('Unsupported subsasgn type.'));
    end
  end
  %=============================================================================
function R = dotSubsasgn(T, sasgn, value)
  R = [];
  if (length(sasgn) == 1)
    if (length(value) == 0 && size(value, 1) == 0 && size(value, 2) == 0)
      st = struct(T);
      st.data = rmfield(st.data, sasgn.subs);
      variableNames = st.Properties.VariableNames;
      removeName = sasgn(1).subs;
      keepIdx = logical(ones(size(variableNames)));
      for i = 1:length(variableNames)
        keepIdx(strcmp(variableNames, removeName)) = false;
      end
      value = variableNames(keepIdx);
      if isrow(value)
        value = value';
      end
      st.Properties.VariableNames = value;     
      R = class(st, 'table');
    else
      st = struct(T);
      if strcmp(sasgn.subs, 'Properties')
        before = st.Properties;
        if (~isstring(before.VariableNames) && ~iscellstr(before.VariableNames))
          error(_('Value assignment must be a cell of characters or a string array.'));
        end
        if ~isequal(size(before.VariableNames), size(value.VariableNames))
          error(_('Value assignment must be same size as existing value.'));
        end
        if (~isstring(before.RowNames) && ~iscellstr(before.RowNames))
          error(_('Value assignment must be a cell of characters or a string array.'));
        end
        if ~isequal(size(before.RowNames), size(value.RowNames))
          error(_('Value assignment must be same size as existing value.'));
        end
        if isstring(value.VariableNames)
          value.VariableNames = cellstr(value.VariableNames);
        end
        if isstring(value.RowNames)
          value.RowNames = cellstr(value.RowNames);
        end
        st.Properties = value;
        R = class(st, 'table'); 
      else
        if isfield(st.data, sasgn.subs)
          before = st.data.(sasgn.subs);
          
          if ~isequal(size(before), size(value))
            error(_('Value assignment must be same size as existing value.'));
          end
          st.data.(sasgn.subs) = value;
          R = class(st, 'table');
        else
          TADD = table(value, 'VariableNames', {sasgn.subs});
          if ~isequal(size(T, 1), size(TADD, 1))
            error(_('Value assignment must be same size as existing value.'));
          end
          R = [T, TADD]; 
        end
      end
    end
  else
    error(_('Unsupported subsasgn type.'));
  end
end
%=============================================================================
function R = braceSubsasgn(T, sasgn, value)
  st = struct(T);
  if (length(sasgn.subs) ~= 2)
    error(_('Unsupported brace indexing format.'));
  end
  idxRow = sasgn.subs{1};
  if ischar(idxRow)
    if strcmp(idxRow, ':')
      idxRow = 1:height(T);
    else
      idxRow = find(strcmp(st.Properties.RowNames, idxRow));
      if isempty(idxRow)
        error(_('Row name not found.'));
      end
    end
  elseif islogical(idxRow)
    idxRow = find(idxRow);
  elseif isnumeric(idxRow)
    % Keep as is
  end
  
  idxCol = sasgn.subs{2};
  if ischar(idxCol)
    if strcmp(idxCol, ':')
      idxCol = 1:width(T);
    else
      idxCol = find(strcmp(st.Properties.VariableNames, idxCol));
      if isempty(idxCol)
        error(_('Column name not found.'));
      end
    end
  elseif islogical(idxCol)
    idxCol = find(idxCol);
  elseif isnumeric(idxCol)
    % Keep as is
  elseif iscellstr(idxCol) || isstring(idxCol)
    idxCol = find(ismember(st.Properties.VariableNames, idxCol));
  end
  
  if (isempty(value) &&  isequal(size(value), [0, 0]) && isdouble(value))
    error(_('To delete rows or variables by assigning [], use () subscripting instead of {}.'));
  end
  
  if istable(value)
    if (height(value) ~= length(idxRow)) || (width(value) ~= length(idxCol))
      error(_('Assigned table must have the same dimensions as the indexed part.'));
    end
    for j = 1:length(idxCol)
      colName = st.Properties.VariableNames{idxCol(j)};
      V =  st.data.(colName);
      V(idxRow, :) = value.data.(colName);
      if ~iscell(V) && ~isstring(V)
        st.data.(colName) = cast(V, 'like', value);
      else
        st.data.(colName) = V;
      end
    end
  else
    if (size(value, 1) ~= length(idxRow))
      error(_('Value assignment must be same size as existing value.'));
    end
    for j = 1:length(idxCol)
      colName = st.Properties.VariableNames{idxCol(j)};
      V = st.data.(colName);
      if (size(value, 2) ~= length(idxCol))
        V(idxRow, :) = value(:, :);
      else
        V(idxRow, :) = value(:, j);
      end
      if ~iscell(V) && ~isstring(V)
        st.data.(colName) = cast(V, 'like', value);
      else
        st.data.(colName) = V;
      end
    end
  end
  R = class(st, 'table');
end
%=============================================================================
function R = parentheseSubsasgn(T, sasgn, value)
  st = struct(T);
  if (length(sasgn.subs) ~= 2)
    error(_('Unsupported parenthese indexing format.'));
  end
  idxRow = sasgn.subs{1};
  if ischar(idxRow) && strcmp(idxRow, ':')
    idxRow = 1:height(T);
  elseif islogical(idxRow)
    idxRow = find(idxRow);
  elseif iscellstr(idxRow) || isstring(idxRow) || ischar(idxRow)
    % Handle row names (if any)
    idxRow = find(ismember(st.Properties.RowNames, idxRow));
  end
  
  idxCol = sasgn.subs{2};
  if ischar(idxCol) && strcmp(idxCol, ':')
    idxCol = 1:width(T);
  elseif isstring(idxCol) || iscellstr(idxCol) || ischar(idxCol)
    idxCol = find(ismember(st.Properties.VariableNames, idxCol));
  elseif islogical(idxCol)
    idxCol = find(idxCol);
  end
  
  if isempty(value) && isequal(size(value), [0, 0]) && isdouble(value)
    % Delete rows or columns
    if ischar(sasgn.subs{2}) && strcmp(sasgn.subs{2}, ':')
      % Delete entire rows
      keepRows = true(height(T), 1);
      keepRows(idxRow) = false;
      for field = fieldnames(st.data)'
        V = st.data.(field{1});
        st.data.(field{1}) = V(keepRows, :);
      end
      if ~isempty(st.Properties.RowNames)
        st.Properties.RowNames = st.Properties.RowNames(keepRows);
      end
    else
      % Delete specific columns
      if ~isequal(idxRow, 1:width(T)) || isempty(idxCol)
        error(_('At least one subscript must be '':'' when you delete rows or variables by assigning [].'));
      end
      for j = idxCol
        colName = st.Properties.VariableNames{j};
        st.data = rmfield(st.data, colName);
      end
      st.Properties.VariableNames(idxCol) = [];
    end
    R = class(st, 'table');
    return
  end
  
  if istable(value)
    if (height(value) ~= length(idxRow)) || (width(value) ~= length(idxCol))
      error(_('Assigned table must have the same dimensions as the indexed part.'));
    end
    stv = struct(value);
    for j = 1:length(idxCol)
      colName = st.Properties.VariableNames{idxCol(j)};
      V = st.data.(colName);
      V(idxRow, :) = stv.data.(colName);
      st.data.(colName) = V;
    end
    R = class(st, 'table');
    return
  end
  
  % Assign value
  if ~isequal(size(value), [length(idxRow), length(idxCol)])
    error(_('Value assignment must be same size as indexed part of table.'));
  end
  
  for j = 1:length(idxCol)
    colName = st.Properties.VariableNames{idxCol(j)};
    V = st.data.(colName);
    V(idxRow,:) = value(:,j);
    st.data.(colName) = V;
  end
  
  R = class(st, 'table');
end
%=============================================================================
