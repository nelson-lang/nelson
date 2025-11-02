%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = sampling(xSeq, className)
  seqLen = length(xSeq);
  if (seqLen == 1)
    seqLen = xSeq;
    xSeq = 1:cast(seqLen, className);
  end
  
  xColumn = xSeq(:);
  num = xColumn;
  den = xColumn - xColumn.';
  samplingMatrix = num ./ den;
  samplingMatrix(1:seqLen+1:seqLen^2) = 0;
  samplingMatrix = samplingMatrix + diag(sum(samplingMatrix));
  A = samplingMatrix;
end
%=============================================================================
