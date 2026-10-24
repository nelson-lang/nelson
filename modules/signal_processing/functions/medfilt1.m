%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = medfilt1(varargin)
  narginchk(1, 6);
  nargoutchk(0, 1);
  input = varargin{1};
  [n, dim, omitNan, truncate] = parseOptions(input, varargin{2:end});
  [x2D, permutationOrder, permutedShape] = reshapeInputAlongDimension(input, dim);
  y2D = medfiltColumns(x2D, n, omitNan, truncate);
  y = ipermute(reshape(y2D, permutedShape), permutationOrder);
end
%=============================================================================
function [n, dim, omitNan, truncate] = parseOptions(input, varargin)
  n = 3;
  dim = [];
  omitNan = false;
  truncate = false;
  if ~isempty(varargin) && ~(ischar(varargin{1}) || isstring(varargin{1}))
    n = varargin{1};
    varargin(1) = [];
  end
  validateWindowLength(n);
  for idx = 1:length(varargin)
    value = varargin{idx};
    if isempty(value)
      continue;
    elseif ischar(value) || isstring(value)
      option = lower(char(value));
      if strcmp(option, 'omitnan')
        omitNan = true;
      elseif strcmp(option, 'includenan')
        omitNan = false;
      elseif strcmp(option, 'truncate')
        truncate = true;
      elseif strcmp(option, 'zeropad')
        truncate = false;
      else
        error('Nelson:signal:unsupportedOption', 'Unsupported option.');
      end
    else
      validateDimension(value);
      dim = value;
    end
  end
  if isempty(dim)
    dim = firstNonSingletonDimension(input);
  end
end
%=============================================================================
function validateWindowLength(n)
  if ~isscalar(n) || ~isfinite(n) || n ~= fix(n) || n <= 0
    error('Nelson:validators:mustBePositiveInteger', '%s must be a positive integer.', 'Window length');
  end
end
%=============================================================================
function validateDimension(dim)
  if ~isscalar(dim) || ~isfinite(dim) || dim ~= fix(dim) || dim <= 0
    error('Nelson:signal:invalidDimension', '%s must be a positive integer scalar.', 'Dimension');
  end
end
%=============================================================================
function dim = firstNonSingletonDimension(x)
  shape = size(x);
  dim = find(shape ~= 1, 1, 'first');
  if isempty(dim)
    dim = 1;
  end
end
%=============================================================================
function [x2D, permutationOrder, permutedShape] = reshapeInputAlongDimension(x, dim)
  shape = size(x);
  numberOfDimensions = max([2, ndims(x), dim]);
  if length(shape) < numberOfDimensions
    shape = [shape, ones(1, numberOfDimensions - length(shape))];
  end
  remainingDimensions = 1:numberOfDimensions;
  remainingDimensions(dim) = [];
  permutationOrder = [dim, remainingDimensions];
  permutedShape = shape(permutationOrder);
  x2D = reshape(permute(x, permutationOrder), permutedShape(1), []);
end
%=============================================================================
function y = medfiltColumns(x, n, omitNan, truncate)
  % every column is filtered at once by the native sliding median.
  before = floor(n / 2);
  after = n - before - 1;
  [rows, cols] = size(x);
  if ~isfloat(x) && ~isinteger(x)
    x = double(x);
  end
  if truncate
    y = __movwindow__(x, before, after, 1, rows, omitNan, 'median');
  else
    padded = [zeros(before, cols); x; zeros(after, cols)];
    y = __movwindow__(padded, before, after, before + 1, before + rows, omitNan, 'median');
  end
end
%=============================================================================
