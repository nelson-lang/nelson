%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = expint(X)
  % real nonnegative data stays on the device; negative or complex values need complex results, done on the host
  narginchk(1, 1);
  if ~isfloat(X)
    error('Nelson:expint:invalidType', 'X must be a single or double array.');
  end
  if isreal(X) && ~any(X(:) < 0)
    Y = nelson.special_functions.internal.expintCore(X);
  else
    Y = __gpuArray_host__('expint', X);
  end
end
%=============================================================================
