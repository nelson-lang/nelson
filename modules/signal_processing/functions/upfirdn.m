%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = upfirdn(varargin)
  narginchk(2, 5);
  nargoutchk(0, 1);
  x = varargin{1};
  h = varargin{2};
  p = 1;
  q = 1;
  dim = [];
  if nargin >= 3
    p = varargin{3};
  end
  if nargin >= 4
    q = varargin{4};
  end
  if nargin == 5
    dim = varargin{5};
  end
  if issparse(x) || issparse(h)
    error('Nelson:signal:upfirdn:sparseNotSupported', 'Sparse matrices are not supported.');
  end
  if isempty(x)
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Input signal');
  end
  if isempty(h)
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Filter coefficients');
  end
  mustBeNumeric(p);
  mustBeScalarOrEmpty(p);
  mustBeFinite(p);
  mustBeInteger(p);
  mustBePositive(p);
  mustBeNumeric(q);
  mustBeScalarOrEmpty(q);
  mustBeFinite(q);
  mustBeInteger(q);
  mustBePositive(q);
  if isempty(dim)
    if isrow(x) && ~isvector(h)
      x = x(:);
      dim = 1;
    end
    dim = find(size(x) ~= 1, 1, 'first');
    if isempty(dim)
      dim = 1;
    end
  else
    mustBeNumeric(dim);
    mustBeScalarOrEmpty(dim);
    mustBeFinite(dim);
    mustBeInteger(dim);
    mustBePositive(dim);
  end
  p = double(p);
  q = double(q);
  dim = double(dim);
  if dim == 1 || dim == 2
    y = builtin('__upfirdn__', x, h, p, q, dim);
    return;
  end
  xu = upsampleWithoutTrailingZeros(x, p, dim);
  yf = fullConvolveAlongDimension(xu, h, dim);
  y = downsample(yf, q, 0, dim);
end
%=============================================================================
function y = upsampleWithoutTrailingZeros(x, p, dim)
  if p == 1
    y = x;
    return;
  end
  if dim == 1
    outputLength = (size(x, 1) - 1) * p + 1;
    y = zeros(outputLength, size(x, 2));
    y(1:p:end, :) = x;
  elseif dim == 2
    outputLength = (size(x, 2) - 1) * p + 1;
    y = zeros(size(x, 1), outputLength);
    y(:, 1:p:end) = x;
  else
    error('Nelson:signal:dimensionMustBe1Or2', 'Dimension must be 1 or 2.');
  end
end
%=============================================================================
function y = fullConvolveAlongDimension(x, h, dim)
  h = h(:);
  if dim == 1
    y = zeros(size(x, 1) + length(h) - 1, size(x, 2));
    for col = 1:size(x, 2)
      y(:, col) = conv(x(:, col), h);
    end
  elseif dim == 2
    y = zeros(size(x, 1), size(x, 2) + length(h) - 1);
    for row = 1:size(x, 1)
      y(row, :) = conv(x(row, :), h.');
    end
  else
    error('Nelson:signal:dimensionMustBe1Or2', 'Dimension must be 1 or 2.');
  end
end
%=============================================================================
