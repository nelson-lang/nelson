%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = vecnorm (varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  
  A = varargin{1};
  if (nargin > 1)
    p = varargin{2};
  else
    p = 2;
  end
  if (nargin > 2)
    dim = varargin{3};
  else
    dim = find(size (A) > 1, 1);
  end
  
  mustBeNumeric(A, 1);
  mustBePositive(p, 2);
  mustBePositive(dim, 3);
  
  if p == 1
    n = sum(abs (A), dim);
  elseif p == 2
    n = sqrt(sum (A .* conj (A), dim));
  elseif isinf(p)
    n = max(abs (A), [], dim);
  else
    if (rem (p, 2) == 0)
      if (iscomplex (A))
        n = (sum ((real (A).^2 + imag (A).^2) .^ (p/2), dim)) .^ (1 / p);
      else
        n = (sum (A.^2 .^ (p/2), dim)) .^ (1 / p);
      end
    else
      n = (sum (abs (A) .^ p, dim)) .^ (1 / p);
    end
  end
end
