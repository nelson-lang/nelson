%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = sqrtm(A)
  % principal square root by the Denman-Beavers iteration on the device; a matrix whose root is
  % not real, or that does not converge, keeps the host answer
  [X, ok] = __gpuArray_sqrtm__(A);
  if ~ok
    X = __gpuArray_host__('sqrtm', A);
  end
end
%=============================================================================
