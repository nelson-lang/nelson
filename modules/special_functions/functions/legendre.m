%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function P = legendre(n, X, normalization)
  % associated Legendre functions of degree n and orders 0..n evaluated at X
  narginchk(2, 3);
  if nargin < 3
    normalization = 'unnorm';
  end
  if ~(isnumeric(n) && isscalar(n) && isreal(n) && n >= 0 && n == fix(n))
    error('Nelson:legendre:invalidDegree', 'N must be a nonnegative integer scalar.');
  end
  n = double(gather(n));
  isText = ischar(normalization) || isstring(normalization);
  if ~(isText && any(strcmpi(char(normalization), {'unnorm', 'sch', 'norm'})))
    error('Nelson:legendre:invalidNormalization', 'Normalization must be ''unnorm'', ''sch'' or ''norm''.');
  end
  normalization = lower(char(normalization));
  if ~isfloat(X) || ~isreal(X) || any(abs(X(:)) > 1)
    error('Nelson:legendre:invalidInput', 'X must be real with values in [-1, 1].');
  end
  x = reshape(X, 1, []);
  s = sqrt((1 - x) .* (1 + x));
  % Q holds sqrt((n-m)!/(n+m)!) * P(n, m), which never overflows
  Q = zeros(n + 1, numel(x), 'like', x);
  diagonal = ones(1, numel(x), 'like', x);
  for m = 0:n
    if m > 0
      diagonal = -sqrt((2 * m - 1) / (2 * m)) * s .* diagonal;
    end
    if m == n
      Q(m + 1, :) = diagonal;
      break
    end
    previous = diagonal;
    current = sqrt(2 * m + 1) * x .* diagonal;
    for l = m + 2:n
      next = ((2 * l - 1) * x .* current - sqrt((l - 1) ^ 2 - m ^ 2) * previous) / sqrt(l ^ 2 - m ^ 2);
      previous = current;
      current = next;
    end
    Q(m + 1, :) = current;
  end
  m = (0:n).';
  switch normalization
    case 'unnorm'
      scale = exp(0.5 * (gammaln(n + m + 1) - gammaln(n - m + 1)));
    case 'sch'
      scale = (-1) .^ m * sqrt(2);
      scale(1) = 1;
    otherwise
      scale = (-1) .^ m * sqrt(n + 0.5);
  end
  P = Q .* cast(scale, underlyingType(x));
  if ~isvector(X)
    P = reshape(P, [n + 1, size(X)]);
  end
end
%=============================================================================
