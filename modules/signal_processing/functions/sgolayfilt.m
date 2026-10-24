%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = sgolayfilt(varargin)
  narginchk(3, 5);
  nargoutchk(0, 1);
  input = varargin{1};
  k = varargin{2};
  f = varargin{3};
  w = [];
  dim = [];
  if nargin >= 4
    w = varargin{4};
  end
  if nargin == 5
    dim = varargin{5};
  end
  if isempty(dim)
    dim = firstNonSingletonDimension(input);
  else
    validateDimension(dim);
  end
  b = sgolay(k, f, w);
  [x2D, permutationOrder, permutedShape] = reshapeInputAlongDimension(input, dim);
  y2D = zeros(size(x2D));
  for col = 1:size(x2D, 2)
    y2D(:, col) = filterVector(x2D(:, col), b, f);
  end
  y = ipermute(reshape(y2D, permutedShape), permutationOrder);
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
function validateDimension(dim)
  if ~isscalar(dim) || ~isfinite(dim) || dim ~= fix(dim) || dim <= 0
    error('Nelson:signal:invalidDimension', '%s must be a positive integer scalar.', 'Dimension');
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
function y = filterVector(x, b, f)
  center = (f + 1) / 2;
  y = zeros(size(x));
  for idx = 1:center - 1
    y(idx) = b(idx, :) * x(1:f);
  end
  for idx = center:length(x) - center + 1
    y(idx) = b(center, :) * x(idx - center + 1:idx + center - 1);
  end
  for idx = length(x) - center + 2:length(x)
    row = f - (length(x) - idx);
    y(idx) = b(row, :) * x(end - f + 1:end);
  end
end
%=============================================================================
