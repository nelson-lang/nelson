%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function A = dramadah(nSize, patternType, className)
  selectedPattern = patternType;
  if isempty(selectedPattern)
    selectedPattern = 1;
  end

  if selectedPattern == 1
    colVec = ones(nSize, 1, className);
    zeroRows = [2:4:nSize, 3:4:nSize];
    if ~isempty(zeroRows)
      colVec(sort(zeroRows)) = 0;
    end
    rowVec = zeros(nSize, 1, className);
    rowSeed = cast([1; 1; 0; 1], className);
    rowVec(1:min(nSize, 4)) = rowSeed(1:min(nSize, 4));
    A = toeplitz(colVec, rowVec);
  elseif selectedPattern == 2
    colVec = zeros(nSize, 1, className);
    if nSize > 0
      colVec(1) = 1;
    end
    rowVec = ones(nSize, 1, className);
    rowVec(3:2:nSize) = 0;
    A = toeplitz(colVec, rowVec);
  elseif selectedPattern == 3
    colVec = ones(nSize, 1, className);
    colVec(2:2:nSize) = 0;
    rowVec = zeros(1, nSize, className);
    rowVec(1:min(nSize, 2)) = 1;
    A = toeplitz(colVec, rowVec);
  end
end
%=============================================================================
