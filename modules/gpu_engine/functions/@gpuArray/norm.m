%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = norm(x, p)
  % Vector norms and the Frobenius norm are computed on the device by composing
  % the gpuArray overloads; the matrix 2-norm (needs the singular values) and
  % any other case fall back to the host.
  if nargin < 2
    p = 2;
  end
  sz = size(x);
  isVector = (length(sz) == 2) && (min(sz) == 1);
  if ischar(p) || isStringScalar(p)
    if strcmpi(char(p), 'fro')
      r = sqrt(sum(abs(x(:)) .^ 2));
      return
    end
  elseif isVector
    if p == 2
      r = sqrt(sum(abs(x) .^ 2));
      return
    elseif p == 1
      r = sum(abs(x));
      return
    elseif isinf(p) && p > 0
      r = max(abs(x));
      return
    elseif isinf(p) && p < 0
      r = min(abs(x));
      return
    elseif p > 0
      r = sum(abs(x) .^ p) .^ (1 / p);
      return
    end
  end
  % matrix p-norm (or an unsupported p): host fallback, re-wrapped as gpuArray
  r = gpuArray(norm(gather(x), p));
end
%=============================================================================
