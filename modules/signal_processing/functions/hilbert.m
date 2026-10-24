%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = hilbert(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  x = varargin{1};
  if nargin == 2
    n = varargin{2};
  else
    n = firstNonSingletonLength(x);
  end
  dim = firstNonSingletonDimension(x);
  xpad = padAlongDimension(x, double(n), dim);
  X = fft(xpad, [], dim);
  h = zeros(n, 1);
  if mod(n, 2) == 0
    h([1, n / 2 + 1]) = 1;
    h(2:n / 2) = 2;
  else
    h(1) = 1;
    h(2:(n + 1) / 2) = 2;
  end
  if dim == 2
    h = h.';
  end
  y = ifft(X .* h);
end
%=============================================================================
function dim = firstNonSingletonDimension(x)
  if size(x, 1) ~= 1
    dim = 1;
  else
    dim = 2;
  end
end
%=============================================================================
function len = firstNonSingletonLength(x)
  dim = firstNonSingletonDimension(x);
  len = size(x, dim);
end
%=============================================================================
function y = padAlongDimension(x, n, dim)
  if dim == 1
    y = zeros(n, size(x, 2));
    count = min(n, size(x, 1));
    y(1:count, :) = x(1:count, :);
  else
    y = zeros(size(x, 1), n);
    count = min(n, size(x, 2));
    y(:, 1:count) = x(:, 1:count);
  end
end
%=============================================================================
