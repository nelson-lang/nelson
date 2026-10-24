%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [b, g] = sgolay(k, f, w)
  narginchk(2, 3);
  nargoutchk(0, 2);
  if ~isscalar(k) || ~isfinite(k) || k ~= fix(k) || k < 0
    error('Nelson:signal:sgolay:invalidOrder', 'Polynomial order must be a nonnegative integer.');
  end
  if ~isscalar(f) || ~isfinite(f) || f ~= fix(f) || f <= 0 || mod(f, 2) == 0
    error('Nelson:signal:sgolay:invalidFrameLength', 'Frame length must be a positive odd integer.');
  end
  if k >= f
    error('Nelson:signal:sgolay:orderTooLarge', 'Polynomial order must be less than frame length.');
  end
  if nargin < 3 || isempty(w)
    w = ones(f, 1);
  else
    if ~isvector(w) || length(w) ~= f || any(w(:) <= 0) || any(~isfinite(w(:)))
      error('Nelson:signal:sgolay:invalidWeights', 'Weights must be a positive finite vector with frame length elements.');
    end
    w = w(:);
  end
  half = (f - 1) / 2;
  x = (-half:half)';
  A = zeros(f, k + 1);
  for p = 0:k
    A(:, p + 1) = x .^ p;
  end
  W = diag(w);
  g = A * inv(A' * W * A);
  b = g * A' * W;
end
%=============================================================================
