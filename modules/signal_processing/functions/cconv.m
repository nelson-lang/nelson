%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = cconv(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  a = varargin{1};
  b = varargin{2};
  if nargin == 3
    n = varargin{3};
  else
    n = max(length(a), length(b));
  end
  validateInputs(a, b, n);
  apad = foldToLength(a(:), n);
  bpad = foldToLength(b(:), n);
  y = ifft(fft(apad) .* fft(bpad));
  if isreal(a) && isreal(b)
    y = real(y);
  end
  if size(a, 1) == 1
    y = y.';
  end
end
%=============================================================================
function validateInputs(a, b, n)
  if ~isvector(a) || ~isvector(b)
    error('Nelson:signal:cconv:vectorExpected', 'A and B must be vectors.');
  end
  if ~isscalar(n) || ~isfinite(n) || n ~= fix(n) || n <= 0
    error('Nelson:signal:cconv:invalidLength', '%s must be a positive integer scalar.', 'Convolution length');
  end
end
%=============================================================================
function y = foldToLength(x, n)
  y = zeros(n, 1);
  for k = 1:length(x)
    index = mod(k - 1, n) + 1;
    y(index) = y(index) + x(k);
  end
end
%=============================================================================
