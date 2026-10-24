%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = chebvand(numRows, pointsSeq, className)
  if isempty(pointsSeq)
    pointSource = numRows;
    useSquareOutput = true;
  else
    pointSource = pointsSeq;
    useSquareOutput = false;
  end

  numPoints = length(pointSource);
  if numPoints == 1
    numPoints = pointSource;
    pointSource = linspace(cast(0, className), 1, numPoints);
  end

  if useSquareOutput
    finalNumRows = numPoints;
  else
    finalNumRows = numRows;
  end

  pointsRow = reshape(pointSource, 1, []);
  C = chebyshevRows(finalNumRows, pointsRow, className);
end
%=============================================================================
function C = chebyshevRows(finalNumRows, pointsRow, className)
  numPoints = length(pointsRow);
  C = ones(finalNumRows, numPoints, className);

  if finalNumRows == 1
    return
  end

  previousRow = C(1, :);
  currentRow = pointsRow;
  C(2, :) = currentRow;
  for idx = 3:finalNumRows
    nextRow = 2 .* pointsRow .* currentRow - previousRow;
    C(idx, :) = nextRow;
    previousRow = currentRow;
    currentRow = nextRow;
  end
end
%=============================================================================
