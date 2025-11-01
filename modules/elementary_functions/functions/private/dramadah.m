%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = dramadah(nSize, patternType, className)
  if isempty(patternType) || (patternType == 1)
    colVec = ones(nSize,1,className);
    for i=2:4:nSize
      segLen = min(1,nSize-i);
      colVec(i:i+segLen) = 0;
    end
    rowVec = zeros(nSize,1,className);
    rowVec(1:4) = [1 1 0 1];
    if nSize < 4
      rowVec = rowVec(1:nSize);
    end
    A = toeplitz(colVec,rowVec);
  elseif patternType == 2
    colVec = zeros(nSize,1,className);
    colVec(1) = 1;
    rowVec = ones(nSize,1,className);
    rowVec(3:2:nSize) = 0;
    A = toeplitz(colVec,rowVec);
  elseif patternType == 3
    colVec = ones(nSize,1,className);
    colVec(2:2:nSize) = 0;
    A = toeplitz(colVec, [1 1 zeros(1,nSize-2)]);
  end
end
%=============================================================================
