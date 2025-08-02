%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = table(varargin)
  nargoutchk(0, 1);
  T = initializeTable();
  if nargin == 0
    varargout{1} = class(T, 'table');
    return
  end
  nextIsRowNames = false;
  nextIsVariableNames = false;
  newVariableNames = {};
  hasVariableNames = find(strcmpi({varargin}, 'VariableNames'));
  namePrefix = inputname(1);
  if (length(namePrefix) == 0)
    namePrefix = 'Var';
  end
  for j = 1:size(varargin, 2)
    name = inputname(j);
    if isDataArgument(name, nextIsRowNames, nextIsVariableNames)
      if (strcmpi(varargin{j}, 'RowNames'))
        nextIsRowNames = true;
      elseif (strcmpi(varargin{j}, 'VariableNames'))
        nextIsVariableNames = true;
      else
        cellCheck = varargin{j};
        cellSize = size(cellCheck);
        if isMultiDimensionalCellArray(cellCheck, cellSize)
          T = assignMultiDimCellData(T, varargin, j, cellSize);
        elseif isRowVectorWithVarNames(cellCheck, cellSize, hasVariableNames)
          T = assignColumnDataToTable(T, namePrefix, varargin, j, cellSize);
        else
          T = assignSingleColumnData(T, namePrefix, varargin, j);
        end
      end
    else
      if (nextIsRowNames)
        T.Properties.RowNames = varargin{j};
        if isstring(T.Properties.RowNames)
          T.Properties.RowNames = cellstr(T.Properties.RowNames);
        end
        nextIsRowNames = false;
      elseif (nextIsVariableNames)
        newVariableNames = varargin{j};
        nextIsVariableNames = false;
      else
        cellCheck = varargin{j};
        cellSize = size(cellCheck);
        if isMultiDimensionalCell(cellCheck, cellSize)
          T = assignMultiDimCellData(T, namePrefix, varargin, j, cellSize);
        elseif isNonEmptyRowVectorWithVarNames(cellCheck, cellSize, hasVariableNames)
          T = assignColumnDataToTable(T, namePrefix, varargin, j, cellSize);
        elseif isSingleColumnCell(cellCheck, cellSize)
          T = assignColumnDataWithDefaultName(T, name, varargin, j);
        else
          T.data.(name) = varargin{j};  
          T.Properties.VariableNames{end+1} = name;
        end
      end
    end
  end
  T = updateVariableNames(T, newVariableNames, namePrefix);
  names = fieldnames(T.data);
  if ~isempty(names)
    szRef = size(T.data.(names{1}), 1);
    for k = 2:length(names')
      if ~isequal(szRef, size(T.data.(names{k}), 1))
        error(_('All table variables must have the same number of rows.'));
      end
    end
  end
  T = updateVariableTypes(T);
  varargout{1} = class(T, 'table');
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
function tf = isDataArgument(name, nextIsRowNames, nextIsVariableNames)
  tf = isempty(name) && ~(nextIsRowNames || nextIsVariableNames);
end
%=============================================================================
function tf = isMultiDimensionalCellArray(cellCheck, cellSize)
  tf = iscell(cellCheck) && ~isempty(cellCheck) && (cellSize(1) ~= 1) && (cellSize(2) ~= 1);
end
%=============================================================================
function tf = isRowVectorWithVarNames(cellCheck, cellSize, hasVariableNames)
  tf = iscell(cellCheck) && ~isempty(cellCheck) && (cellSize(1) == 1) && (cellSize(2) ~= 1) && ~isempty(hasVariableNames);
end
%=============================================================================
function tf = isMultiDimensionalCell(cellCheck, cellSize)
  tf = iscell(cellCheck) && (cellSize(1) ~= 1) && (cellSize(2) ~= 1);
end
%=============================================================================
function tf = isNonEmptyRowVectorWithVarNames(cellCheck, cellSize, hasVariableNames)
  tf = iscell(cellCheck) && (cellSize(1) == 1) && (cellSize(2) ~= 1) && ~isempty(hasVariableNames);
end
%=============================================================================
function tf = isSingleColumnCell(cellCheck, cellSize)
  tf = iscell(cellCheck) && (cellSize(2) == 1);
end
%=============================================================================
function T = assignColumnDataToTable(T, namePrefix, args, j, cellSize)
  for k = 1:cellSize(2)
    col_name = [namePrefix, num2str(j + k - 1)];
    T.Properties.VariableNames{end+1} = col_name;
    v = args{j};
    temp = v(:, k);
    if (isnumeric(temp{1}) || islogical(temp{1}))
      try
        T.data.(col_name) = cell2mat(temp);
      catch
        T.data.(col_name) = temp;
      end
    else
      T.data.(col_name) = temp;
    end
  end
end
%=============================================================================
function T = assignSingleColumnData(T, namePrefix, args, j)
  col_name = [namePrefix, num2str(j)];
  T.Properties.VariableNames{end+1} = col_name;
  temp = args{j};
  if (iscell(temp) && ~isempty(temp) && (isnumeric(temp{1}) || islogical(temp{1})))
    T.data.(col_name) = cell2mat(args{j});
  else
    T.data.(col_name) = args{j};
  end
end
%=============================================================================
function T = assignMultiDimCellData(T, namePrefix, args, j, cellSize)
  for k=1:cellSize(2)
    col_name = [namePrefix, num2str(j + k - 1)];
    T.Properties.VariableNames{end+1} = col_name;
    v = args{j};
    temp = v(:, k);
    if (isnumeric(temp{1}) || islogical(temp{1}))
      try
        T.data.(col_name) = cell2mat(temp);
      catch
        T.data.(col_name) = temp;
      end
    else
      T.data.(col_name) = temp;
    end
  end
end
%=============================================================================
function T = assignColumnDataWithDefaultName(T, name, args, j)
  if (length(name) == 0)
    col_name = 'Var1';
  else
    col_name = name;
  end
  T.Properties.VariableNames{end+1} = col_name;
  temp = args{j};
  if (isnumeric(temp{1}) || islogical(temp{1}))
    T.data.(col_name) = cell2mat(temp);
  else
    T.data.(col_name) = temp;
  end
end
%=============================================================================
function T = updateVariableNames(T, newVariableNames, namePrefix)
  T.Properties.RowNames = T.Properties.RowNames(:)';
  if (length(newVariableNames) ~= 0)
    if (length(newVariableNames) ~= length(fieldnames(T.data)))
      error(_('Mismatched variable names.'));
    end
    
    newdata = struct();
    
    for j=1:length(newVariableNames)
      if (length(T.Properties.VariableNames) ~= 0)
        col_name = T.Properties.VariableNames{j};
      else
        col_name = [namePrefix, num2str(j)];
      end
      
      value = T.data.(col_name);
      if iscell(value)
        all_strings = all(cellfun(@isstring, value));
        if (all_strings)
          value = string(value);
        end
      end
      newdata.(newVariableNames{j}) = value;
      
    end
    
    T.data = newdata;
    T.Properties.VariableNames = newVariableNames;
  end
end
%=============================================================================
function T = initializeTable()
  T = struct();
  T.data = struct();
  T.Version = 1;
  T.Properties = struct();
  T.Properties.VariableNames = {};
  T.Properties.VariableTypes = string([]);
  T.Properties.RowNames = {};
end
%=============================================================================
