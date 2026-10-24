%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = randg(varargin)
  % randg generates gamma-distributed random numbers with unit scale.
  %   randg            scalar with shape 1 (a unit-mean exponential)
  %   randg(A)         array the size of A, element (i) having shape A(i)
  %   randg(A, n)      n-by-n array, shape A (A scalar)
  %   randg(A, m, n)   m-by-n array, shape A (A scalar)
  %   randg(A, [m n])  size [m n], shape A (A scalar)
  %   randg(..., classname)  'single' or 'double' (default 'double')
  nargoutchk(0, 1);

  className = 'double';
  args = varargin;
  if ~isempty(args) && (ischar(args{end}) || isStringScalar(args{end}))
    className = lower(char(args{end}));
    if ~any(strcmp(className, {'single', 'double'}))
      error('Nelson:randg:invalidClass', 'CLASSNAME must be ''single'' or ''double''.');
    end
    args(end) = [];
  end

  if isempty(args)
    A = 1;
  else
    A = args{1};
    args(1) = [];
  end
  if ~isnumeric(A) || ~isreal(A) || any(A(:) < 0)
    error('Nelson:randg:invalidShape', 'A must be a real array of nonnegative shape parameters.');
  end

  if isempty(args)
    if isscalar(A)
      shapes = double(A);
      sz = [1 1];
    else
      shapes = double(A(:));
      sz = size(A);
    end
  else
    if ~isscalar(A)
      error('Nelson:randg:scalarShapeExpected', ...
        'A must be a scalar when a size is supplied.');
    end
    sz = randgSizeFromArgs(args);
    shapes = repmat(double(A), prod(sz), 1);
  end

  n = prod(sz);
  values = randgGammaSamples(shapes, n);
  r = reshape(values, sz);
  if strcmp(className, 'single')
    r = single(r);
  end
end
%=============================================================================
function sz = randgSizeFromArgs(args)
  if numel(args) == 1
    d = args{1};
    if ~isnumeric(d) || ~isreal(d)
      error('Nelson:randg:invalidSize', 'Size inputs must be real integers.');
    end
    if isscalar(d)
      sz = [double(d) double(d)];
    else
      sz = double(d(:)');
    end
  else
    sz = zeros(1, numel(args));
    for k = 1:numel(args)
      d = args{k};
      if ~isnumeric(d) || ~isreal(d) || ~isscalar(d)
        error('Nelson:randg:invalidSize', 'Size inputs must be real scalar integers.');
      end
      sz(k) = double(d);
    end
  end
  sz = floor(sz);
  sz(sz < 0) = 0;
  if isscalar(sz)
    sz = [sz sz];
  end
end
%=============================================================================
function g = randgGammaSamples(shapes, n)
  % Marsaglia and Tsang's method. shapes is a column vector of length n (one
  % shape per output element); n is the element count.
  g = zeros(n, 1);
  if n == 0
    return
  end
  zeroShape = (shapes == 0);
  if any(zeroShape)
    g(zeroShape) = 0;
  end
  small = (shapes > 0) & (shapes < 1);
  big = (shapes >= 1);
  if any(big)
    g(big) = randgMarsagliaTsang(shapes(big));
  end
  if any(small)
    % Boost a sub-one shape to shape+1, then scale by u^(1/a).
    boosted = randgMarsagliaTsang(shapes(small) + 1);
    u = rand(sum(small), 1);
    g(small) = boosted .* u .^ (1 ./ shapes(small));
  end
end
%=============================================================================
function out = randgMarsagliaTsang(a)
  % a is a column vector of shape parameters, each >= 1. Vectorized rejection:
  % only the still-rejected elements are resampled on each pass.
  m = numel(a);
  out = zeros(m, 1);
  d = a - 1 / 3;
  c = 1 ./ sqrt(9 * d);
  pending = (1:m)';
  while ~isempty(pending)
    dp = d(pending);
    cp = c(pending);
    x = randn(numel(pending), 1);
    v = (1 + cp .* x) .^ 3;
    u = rand(numel(pending), 1);
    accept = (v > 0) ...
      & (log(u) < 0.5 * x .^ 2 + dp - dp .* v + dp .* log(max(v, realmin)));
    out(pending(accept)) = dp(accept) .* v(accept);
    pending = pending(~accept);
  end
end
%=============================================================================
