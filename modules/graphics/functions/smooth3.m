%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function W = smooth3(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [V, method, windowSize, sd] = parseSmooth3Inputs(args);
  W = double(V);
  weights = smooth3KernelWeights(method, windowSize, sd);
  for dim = 1:3
    W = smooth3ApplyWeights(W, weights{dim}, dim);
  end
end
%=============================================================================
