%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = mpower(A, B)
  % A^p for a whole p by repeated squaring on the device; other cases run on the host
  if isscalar(A) && isscalar(B)
    C = A .^ B;
    return
  end
  if isgpuarray(B) && isscalar(B)
    B = gather(B);
  end
  if ~isgpuarray(A) && isfloat(A)
    A = gpuArray(A);
  end
  if isgpuarray(A) && isfloat(A) && isreal(A) && ismatrix(A) && size(A, 1) == size(A, 2) ...
      && isnumeric(B) && isscalar(B) && isreal(B) && isfinite(B) && B == fix(B)
    p = abs(double(B));
    if B < 0
      X = inv(A);
    else
      X = A;
    end
    C = [];
    while p > 0
      if mod(p, 2) == 1
        if isempty(C)
          C = X;
        else
          C = C * X;
        end
      end
      p = floor(p / 2);
      if p > 0
        X = X * X;
      end
    end
    if isempty(C)
      C = eye(size(A, 1), 'like', A);
    end
    return
  end
  C = __gpuArray_host__('mpower', A, B);
end
%=============================================================================
