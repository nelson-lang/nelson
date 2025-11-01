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
  % Resolve points sequence, determine final numRows/numPoints
  [pointsRow, numPoints, finalNumRows] = resolve_points_seq(numRows, pointsSeq, className);
  
  % Build Chebyshev-Vandermonde matrix
  C = build_chebvand(finalNumRows, pointsRow, numPoints, className);
end
%=============================================================================
function [pointsRow, numPoints, finalNumRows] = resolve_points_seq(numRows, pointsSeq, className)
  if isempty(pointsSeq)
    pointsSeq = numRows;
    square = 1;
  else
    square = 0;
  end
  numPoints = length(pointsSeq);
  
  if numPoints == 1
    numPoints = pointsSeq;
    pointsSeq = linspace(cast(0, className), 1, numPoints);
  end
  
  if square == 1
    finalNumRows = numPoints;
  else
    finalNumRows = numRows;
  end
  
  pointsRow = pointsSeq(:).';
end
%=============================================================================
function C = build_chebvand(finalNumRows, pointsRow, numPoints, className)
  C = ones(finalNumRows, numPoints, className);
  
  if finalNumRows == 1
    return
  end
  
  C(2,:) = pointsRow;
  for idx = 3:finalNumRows
    C(idx,:) = 2 .* pointsRow .* C(idx-1,:) - C(idx-2,:);
  end
end
%=============================================================================
