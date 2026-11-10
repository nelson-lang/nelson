%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = psi(varargin)
  % digamma (k = 0) or polygamma of order k of a real nonnegative array
  narginchk(1, 2);
  if nargin == 1
    k = 0;
    X = varargin{1};
  else
    k = varargin{1};
    X = varargin{2};
    if ~(isnumeric(k) && isscalar(k) && isreal(k) && k >= 0 && k == fix(k))
      error('Nelson:psi:invalidOrder', 'K must be a nonnegative integer scalar.');
    end
    k = double(gather(k));
  end
  if ~isfloat(X)
    error('Nelson:psi:invalidType', 'X must be a single or double array.');
  end
  if ~isreal(X)
    error('Nelson:psi:notReal', 'X must be real.');
  end
  if any(X(:) < 0)
    error('Nelson:psi:negative', 'X must be nonnegative.');
  end
  if strcmp(class(X), 'single')
    Y = single(psi(k, double(X)));
    return
  end
  % shift so that the asymptotic series is accurate, then step back with the recurrence
  shift = ceil(10 + 1.5 * k);
  small = X < shift;
  xs = X(small);
  z = X;
  z(small) = xs + shift;
  bernoulli = [1/6, -1/30, 1/42, -1/30, 5/66, -691/2730, 7/6, -3617/510];
  z2 = 1 ./ (z .* z);
  if k == 0
    Y = log(z) - 0.5 ./ z;
    zp = ones(size(z), 'like', z);
    for j = 1:numel(bernoulli)
      zp = zp .* z2;
      Y = Y - (bernoulli(j) / (2 * j)) * zp;
    end
    correction = zeros(size(xs), 'like', xs);
    for j = shift - 1:-1:0
      correction = correction + 1 ./ (xs + j);
    end
    Y(small) = Y(small) - correction;
    return
  end
  sgn = (-1) ^ (k + 1);
  Y = factorial(k - 1) ./ z .^ k + (factorial(k) / 2) ./ z .^ (k + 1);
  zp = 1 ./ z .^ k;
  for j = 1:numel(bernoulli)
    zp = zp .* z2;
    Y = Y + (bernoulli(j) * exp(gammaln(2 * j + k) - gammaln(2 * j + 1))) * zp;
  end
  Y = sgn * Y;
  scale = sgn * factorial(k);
  correction = zeros(size(xs), 'like', xs);
  for j = shift - 1:-1:0
    correction = correction + 1 ./ (xs + j) .^ (k + 1);
  end
  Y(small) = Y(small) + scale * correction;
  Y(X == Inf) = Inf;
end
%=============================================================================
