
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = pow2(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if (nargin == 2)
    X = varargin{1};
    E = varargin{2};
    doComplexWarning = false;
    if ~isreal(X)
      X = real(X);
      doComplexWarning = true;
    end
    if ~isreal(E)
      E = real(E);
      doComplexWarning = true;
    end
    R = scaleByPowerOfTwo(X, E);
    if doComplexWarning
      warning('Nelson:pow2:ignoredImagPart', 'Imaginary part ignored.');
    end
  else
    Y = varargin{1};
    R = 2 .^ Y;
  end
end
%=============================================================================
function R = scaleByPowerOfTwo(X, E)
  % X .* 2^fix(E): the exponent is truncated, and the scaling is split in two
  % halves so that 2^E itself never overflows or underflows before X is scaled.
  E = fix(E);
  E1 = fix(E / 2);
  E2 = E - E1;
  E2(~isfinite(E)) = 0;
  R = (X .* (2 .^ E1)) .* (2 .^ E2);
  % an infinite exponent scales a zero to itself, and an infinite value is
  % unchanged by any exponent but NaN (not NaN)
  unchanged = ((X == 0) & isinf(E)) | (isinf(X) & ~isnan(E));
  if any(unchanged(:))
    if isscalar(X)
      X = repmat(X, size(unchanged));
    end
    R(unchanged) = X(unchanged);
  end
end
%=============================================================================
