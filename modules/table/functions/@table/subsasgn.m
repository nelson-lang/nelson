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
      variableTypes = st.Properties.VariableTypes;
      removeName = sasgn(1).subs;
      keepIdx = logical(ones(size(variableNames)));
      for i = 1:length(variableNames)
        keepIdx(strcmp(variableNames, removeName)) = false;
      end
      value = variableNames(keepIdx);
      types = variableTypes(keepIdx);
      if isrow(value)
        value = value';
      end
      if isrow(types)
        types = types';
      end
      st.Properties.VariableNames = value;   
      st.Properties.VariableTypes = types;  
      R = class(st, 'table');
    else
      st = struct(T);
      if strcmp(sasgn.subs, 'Properties')
        R = updateProperties(st, value); 
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
function R = updateProperties(st, value)
  before = st.Properties;
  if isequal(fieldnames(st), fieldnames(value))
    error(_('Same property names expected.'));
  end
  
  if (~isstring(before.VariableNames) && ~iscellstr(before.VariableNames))
    error(_('Value assignment must be a cell of characters or a string array.'));
  end
  if ~isequal(size(before.VariableNames), size(value.VariableNames))
    error(_('Value assignment must be same size as existing value.'));
  end
  if (~isstring(before.RowNames) && ~iscellstr(before.RowNames))
    error(_('Value assignment must be a cell of characters or a string array.'));
  end
  if ~isempty(before.RowNames) && ~isequal(size(before.RowNames), size(value.RowNames))
    error(_('Value assignment must be same size as existing value.'));
  end
  if iscellstr(value.VariableTypes)
    value.VariableTypes = string(value.VariableTypes);
  end
  if ~isstring(value.VariableTypes)
    error(_('VariableTypes must be a row string vector.'));
  end
  if ~isequal(value.VariableTypes, before.VariableTypes)
    if  ~isequal(size(value.VariableTypes), size(before.VariableTypes))
      error(_('VariableTypes must be same size as existing value.'));
    end
    st = updateTypes(st, value.VariableTypes);
  end
  st.Properties.RowNames = value.RowNames;
  T = class(st, 'table'); 
  ce = {};
  if isstring(value.VariableNames) || iscellstr(value.VariableNames)
    if iscellstr(value.VariableNames)
      ce = value.VariableNames;
    else 
      ce = cellstr(value.VariableNames);
    end
    R = renamevars(T, before.VariableNames, ce);
    return
  else 
    if isstring(value.RowNames)
      value.RowNames = cellstr(value.RowNames);
    end
  end
  st.Properties = value;
  R = class(st, 'table');
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
  st = updateVariableTypes(st);
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
      if ~isequal(idxRow, 1:height(T)) || isempty(idxCol)
        error(_('At least one subscript must be '':'' when you delete rows or variables by assigning [].'));
      end
      for j = idxCol
        colName = st.Properties.VariableNames{j};
        st.data = rmfield(st.data, colName);
      end
      st.Properties.VariableNames(idxCol) = [];
      st.Properties.VariableTypes(idxCol) = [];
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
    st = updateVariableTypes(st);
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
  st = updateVariableTypes(st);
  R = class(st, 'table');
end
%=============================================================================
function st = updateVariableTypes(st)
  newVariableTypes = string([]);
  for j = 1:length(st.Properties.VariableNames)
    colName = st.Properties.VariableNames{j};
    newVariableTypes(end + 1) = class(st.data.(colName));
  end
  st.Properties.VariableTypes = newVariableTypes;
end
%=============================================================================
function st = updateTypes(st, newVariableTypes)
  for j = 1:length(st.Properties.VariableNames)
    colName = st.Properties.VariableNames{j};
    V = st.data.(colName);
    if ~strcmp(class(st.data.(colName)), newVariableTypes(j))
      st.data.(colName) = cast(V, newVariableTypes(j));
    end
  end
  st.Properties.VariableTypes = newVariableTypes;
end
%=============================================================================
