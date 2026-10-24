%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function celldisp(c, s)
  narginchk(1, 2);
  nargoutchk(0, 0);
  mustBeA(c, 'cell', 1);

  if (nargin == 2)
    mustBeTextScalar(s, 2);
    displayName = convertStringsToChars(s);
  else
    displayName = inputname(1);
  end

  fmt = format();
  isloose = (fmt.LineSpacing == "loose");

  if isempty(displayName)
    displayName = 'ans';
  end

  cellShape = size(c);
  for linearIndex = 1:numel(c)
    cellValue = c{linearIndex};
    elementName = [displayName, subsCellPart(linearIndex, cellShape)];
    if iscell(cellValue) && ~isempty(cellValue)
      celldisp(cellValue, elementName)
    else
      displayCellValue(elementName, cellValue, isloose);
    end
  end
end
%=============================================================================
function displayCellValue(elementName, cellValue, isloose)
  blankLine(isloose)
  fprintf('%s =\n', elementName)
  blankLine(isloose)
  if isempty(cellValue)
    displayEmptyCellValue(cellValue);
  else
    disp(cellValue)
  end
  blankLine(isloose);
end
%=============================================================================
function displayEmptyCellValue(cellValue)
  if iscell(cellValue)
    disp('     {}')
  elseif ischar(cellValue)
    disp([blanks(5), char([39, 39])])
  elseif isnumeric(cellValue)
    disp('     []')
  else
    rowCount = size(cellValue, 1);
    columnCount = size(cellValue, 2);
    fprintf([_('%0.f-by-%0.f %s'), newline], rowCount, columnCount, class(cellValue));
  end
end
%=============================================================================
function blankLine(isloose)
  if (isloose)
    disp(' ')
  end
end
%=============================================================================
function text = subsCellPart(linearIndex, cellShape)
  if length(cellShape) == 2 && any(cellShape == 1)
    subscriptValues = linearIndex;
  else
    subscriptValues = linearSubscripts(linearIndex, cellShape);
  end

  text = sprintf('{%d', subscriptValues(1));
  for subscriptIndex = 2:length(subscriptValues)
    text = sprintf('%s,%d', text, subscriptValues(subscriptIndex));
  end
  text = sprintf('%s}', text);
end
%=============================================================================
function subscriptValues = linearSubscripts(linearIndex, cellShape)
  subscriptValues = ones(1, length(cellShape));
  zeroBasedIndex = linearIndex - 1;
  for dimensionIndex = 1:length(cellShape)
    subscriptValues(dimensionIndex) = rem(zeroBasedIndex, cellShape(dimensionIndex)) + 1;
    zeroBasedIndex = fix(zeroBasedIndex / cellShape(dimensionIndex));
  end
end
%=============================================================================
