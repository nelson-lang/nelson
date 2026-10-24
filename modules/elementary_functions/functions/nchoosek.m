%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = nchoosek(v, k)
  narginchk(2, 2);
  nargoutchk(0, 1);
  if ~isscalar(k) || ~isreal(k) || (double(k) ~= fix(double(k))) || (double(k) < 0)
    error('Nelson:elementary_functions:kNonNegativeInteger', 'K must be a nonnegative integer.');
  end
  if isscalar(v) && ~ischar(v) && ~islogical(v) && isreal(v) && (double(v) == fix(double(v))) && (double(v) >= 0)
    % scalar nonnegative integer: binomial coefficient
    C = binomialCoefficient(v, k);
  else
    k = double(k);
    % vector (or non-integer scalar treated as a one-element vector):
    % matrix of all k-combinations, rows in the expected (lexicographic) order
    if ~isvector(v)
      error('Nelson:elementary_functions:firstInputScalarVector', 'First input must be a scalar or a vector.');
    end
    n = numel(v);
    v = reshape(v, 1, n);
    if k > n
      C = reshape(v([]), 0, k);
    elseif k == 0
      C = reshape(v([]), 1, 0);
    elseif k == n
      C = v;
    else
      idx = local_combinations_indices(n, k);
      C = reshape(v(idx), size(idx));
    end
  end
end
%=============================================================================
function C = binomialCoefficient(v, k)
  % result class: the integer class of N or K, else single if any, else double
  if isinteger(v) || isinteger(k)
    mixedClasses = (isinteger(v) && isinteger(k) && ~strcmp(class(v), class(k))) ...
      || (isinteger(v) && (isa(k, 'single') || islogical(k))) || (isinteger(k) && isa(v, 'single'));
    if mixedClasses
      error('Nelson:elementary_functions:integersSameClass', 'Integers can only be combined with integers of the same class, or scalar doubles.');
    end
    if isinteger(v)
      outClass = class(v);
    else
      outClass = class(k);
    end
  elseif isa(v, 'single') || isa(k, 'single')
    outClass = 'single';
  else
    outClass = 'double';
  end
  n = double(v);
  k = double(k);
  if k > n
    error('Nelson:elementary_functions:kIntegerBetween0N', 'K must be an integer between 0 and N.');
  end
  if isfloat(v) && n > flintmax
    error('Nelson:elementary_functions:nMaximumFlintmax', 'When N is single or double, its maximum allowed value is FLINTMAX.');
  end
  kk = min(k, n - k);
  if strcmp(outClass, 'int64') || strcmp(outClass, 'uint64')
    C = binomialCoefficient64(n, kk, outClass);
    return
  end
  % c * (n - kk + j) / j is the integer C(n - kk + j, j) at every step
  c = 1;
  for j = 1:kk
    c = c * (n - kk + j) / j;
  end
  if ~isfinite(c)
    % the exact running product overflowed before the final division
    c = prod((n - kk + 1:n) ./ (1:kk));
  end
  C = cast(round(c), outClass);
end
%=============================================================================
function C = binomialCoefficient64(n, kk, outClass)
  % 64-bit integers: exact saturated evaluation in the output class.
  % C(n - kk + j, j) never exceeds the result, so the product only saturates
  % when the result itself does.
  c = ones(1, 1, outClass);
  mx = intmax(outClass);
  for j = 1:kk
    jj = cast(j, outClass);
    g = gcd(c, jj);
    c = idivide(c, g) * idivide(cast(n - kk + j, outClass), idivide(jj, g));
    if c == mx
      break
    end
  end
  C = c;
end
%=============================================================================
function idx = local_combinations_indices(n, k)
  % all k-combinations of 1:n in lexicographic order (the expected order)
  m = round(prod((n - k + 1:n) ./ (1:k)));
  idx = zeros(m, k);
  c = 1:k;
  for r = 1:m
    idx(r, :) = c;
    j = k;
    while (j >= 1) && (c(j) == n - k + j)
      j = j - 1;
    end
    if j >= 1
      c(j) = c(j) + 1;
      c(j + 1:k) = c(j) + (1:(k - j));
    end
  end
end
%=============================================================================
