%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function cellArray = table2cell(inputTable)
  narginchk(1, 1); % Ensure exactly one input
  nargoutchk(0, 1); % Ensure no more than one output
  
  % Check if the input is a table
  if ~istable(inputTable)
    error('Input argument must be a table.');
  end
  
  % Get the size of the table (number of rows and columns)
  [numRows, numColumns] = size(inputTable);
  
  % Initialize a cell array of the same size
  cellArray = cell(numRows, numColumns);
  
  % Convert table to struct and extract the 'data' field
  tableStruct = struct(inputTable);
  tableDataCells = struct2cell(tableStruct.data);
  
  % Iterate through each column of the table
  for columnIndex = 1:numColumns
    columnData = tableDataCells{columnIndex};
    
    if iscell(columnData)
      if iscolumn(columnData)
        % Directly assign column if it's a cell array column
        cellArray(:, columnIndex) = columnData;
      else
        % Convert matrix to cells
        cellArray(:, columnIndex) = localMatrixToCell(columnData, ones(numRows, 1), size(columnData, 2));
      end
    else
      % Convert non-cell matrices to cell array
      cellArray(:, columnIndex) = localMatrixToCell(columnData, ones(numRows, 1), size(columnData, 2));
    end
  end
end
%=============================================================================
function cellArray = localMatrixToCell(matrixData, rowSizes, columnSizes)
  numRows = length(rowSizes);
  numColumns = length(columnSizes);
  cellArray = cell(numRows, numColumns);
  rowStartIndex = 0;
  
  % Loop through rows and columns to assign matrix data to cells
  for rowIndex = 1:numRows
    columnStartIndex = 0;
    for columnIndex = 1:numColumns
      cellArray{rowIndex, columnIndex} = matrixData(rowStartIndex+(1:rowSizes(rowIndex)), columnStartIndex+(1:columnSizes(columnIndex)));
      columnStartIndex = columnStartIndex + columnSizes(columnIndex);
    end
    rowStartIndex = rowStartIndex + rowSizes(rowIndex);
  end
end
%=============================================================================
