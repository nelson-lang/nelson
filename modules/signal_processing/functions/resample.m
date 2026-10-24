%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = resample(varargin)
  narginchk(3, 7);
  nargoutchk(0, 1);
  x = varargin{1};
  p = varargin{2};
  q = varargin{3};
  n = 10;
  beta = 5;
  h = [];
  dim = [];
  argumentIndex = 4;
  if argumentIndex <= nargin && ~(ischar(varargin{argumentIndex}) || isstring(varargin{argumentIndex}))
    value = varargin{argumentIndex};
    if isscalar(value)
      n = value;
    else
      h = value(:).';
    end
    argumentIndex = argumentIndex + 1;
  end
  if argumentIndex <= nargin && ~(ischar(varargin{argumentIndex}) || isstring(varargin{argumentIndex}))
    beta = varargin{argumentIndex};
    argumentIndex = argumentIndex + 1;
  end
  while argumentIndex <= nargin
    name = char(varargin{argumentIndex});
    if strcmpi(name, 'Dimension')
      dim = varargin{argumentIndex + 1};
      argumentIndex = argumentIndex + 2;
    else
      error('Nelson:signal:unknownOption', 'Unknown option.');
    end
  end
  if isempty(dim)
    dim = find(size(x) ~= 1, 1, 'first');
    if isempty(dim)
      dim = 1;
    end
  end
  inputLength = size(x, dim);
  g = gcd(p, q);
  p = p / g;
  q = q / g;
  if isempty(h)
    filterLength = 2 * n * max(p, q);
    cutoff = 1 / max(p, q);
    h = p * fir1(filterLength, cutoff, kaiser(filterLength + 1, beta));
  end
  filterDelay = ceil((length(h) - 1) / 2);
  phase = mod(filterDelay, q);
  % upfirdn decimates on multiples of q while the wanted samples sit at
  % filterDelay modulo q: a few zero samples ahead of the signal move them onto
  % that grid, so only the samples kept after decimation are ever filtered.
  leadingSamples = 0;
  if q > 1 && phase > 0
    leadingSamples = find(mod((0:q - 1) * p + phase, q) == 0, 1) - 1;
  end
  if leadingSamples > 0 && isnumeric(x)
    x = prependZeroSamples(x, leadingSamples, dim);
  end
  y = upfirdn(x, h, p, q, dim);
  delay = (filterDelay + leadingSamples * p) / q;
  outputLength = ceil(inputLength * p / q);
  y = trimResampledOutput(y, delay, outputLength, dim);
end
%=============================================================================
function x = prependZeroSamples(x, count, dim)
  if dim == 1
    x = [zeros(count, size(x, 2)); x];
  elseif dim == 2
    x = [zeros(size(x, 1), count), x];
  else
    error('Nelson:signal:dimensionMustBe1Or2', 'Dimension must be 1 or 2.');
  end
end
%=============================================================================
function y = trimResampledOutput(y, delay, outputLength, dim)
  first = delay + 1;
  last = min(first + outputLength - 1, size(y, dim));
  if dim == 1
    y = y(first:last, :);
  elseif dim == 2
    y = y(:, first:last);
  else
    error('Nelson:signal:dimensionMustBe1Or2', 'Dimension must be 1 or 2.');
  end
end
%=============================================================================
