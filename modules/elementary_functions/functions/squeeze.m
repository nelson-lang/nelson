%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = squeeze(A)
  if ismatrix(A)
    B = A;
    return
  end
  outputShape = size(A);
  outputShape(outputShape == 1) = [];
  B = reshape(A, matrixShape(outputShape));
end
%=============================================================================
function outputShape = matrixShape(outputShape)
  if isscalar(outputShape)
    outputShape = [outputShape, 1];
  end
end
%=============================================================================
