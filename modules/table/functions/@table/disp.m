%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function disp(varargin)
  narginchk(1, 1);
  T = varargin{1};
  if isempty(T)
    return;
  end
  currentFormat = format();
  isLineSpacing = strcmp(currentFormat.LineSpacing, 'loose');
  
  varNames = T.Properties.VariableNames;
  numCols = width(T);
  numRows = height(T);

  haveRowsNames = ~isempty(T.Properties.RowNames);

  % Display Header
  strs = string(cell(numRows + 1, numCols + 1));
  for j = 1:numCols
    for i = 1:numRows
      t = T{i,j};
      value = t;
      is2d = length(size(value));
      if is2d < 3
        isString = isa(value, 'string');
        value = formattedDisplayText(value);
        if isString
          value = """" + value + """";
        end
        value = char(value);
        value = strtrim(value(1:end));
        if length(value) > 15
          strs{i + 1, j + 1} = sizeAsString(t);
        else
           strs{i + 1, j + 1} = value;
        end
      else
        strs{i + 1, j + 1} = sizeAsString(value);
      end
    end
  end
  
  % Add variable names
  for j = 1:numCols
    strs{1, j+1} = varNames{j};
  end
  
  % Add row names
  if haveRowsNames
    for i = 1:numRows
      strs(i + 1, 1) = T.Properties.RowNames{i};
    end
  end
  
  numCols = size(strs, 2);

  % Create a new row with empty strings, matching the number of columns in A
  newRow = strings(1, numCols);
  
  % Insert the new row between the rows of A
  strs = [strs(1,:); newRow; newRow; strs(2:end,:)];

  maxLenPerCol = max(strlength(strs), [], 1);
  if haveRowsNames
    startIndex = 2;
  else
    startIndex = 1;
  end

  for j = startIndex:numCols
    strs(2, j) = repmat('_', 1, maxLenPerCol(j));
  end

  % Initialize the adjusted array with the same size
  B = strings(size(strs));
  
  % Adjust each string in the array to be centered based on the column's max length of each column
  for j = 1:size(strs, 2)
    for i = 1:size(strs, 1)
      nbBlanks = maxLenPerCol(j) - strlength(strs(i,j));
      B(i,j) =  strjust([blanks(nbBlanks), strs{i,j}], 'left');
    end
  end
  
  blanksSeparator = '    ';
  if haveRowsNames
    startIndex = 1;
  else
    startIndex = 2;
  end
  for i = 1:size(B, 1)
    line = blanksSeparator;
    for j = startIndex:size(B, 2)
      line = line + B(i, j) + blanksSeparator;
    end
    disp(line);
  end

  if isLineSpacing
    disp(' ');
  end
end
%=============================================================================
function s = sizeAsString(value)
  sz = size(value);
  sizeStr = sprintf('%dx', sz(1:end));
  sizeStr = sizeStr(1:end-1);
  s = [sizeStr, ' ', class(value)];
end
%=============================================================================
