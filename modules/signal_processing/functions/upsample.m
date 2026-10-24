%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = upsample(varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);

  x = varargin{1};
  factor = varargin{2};
  phase = 0;
  dim = [];

  if nargin >= 3
    phase = varargin{3};
  end
  if nargin == 4
    dim = varargin{4};
  end

  mustBeNumeric(factor);
  mustBeScalarOrEmpty(factor);
  mustBeFinite(factor);
  mustBeInteger(factor);
  mustBePositive(factor);

  mustBeNumeric(phase);
  mustBeScalarOrEmpty(phase);
  mustBeFinite(phase);
  mustBeInteger(phase);
  mustBeGreaterThanOrEqual(phase, 0);
  mustBeLessThan(phase, factor);

  if isempty(dim)
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

  factor = double(factor);
  phase = double(phase);
  dim = double(dim);

  shape = size(x);
  numberOfDimensions = max([2, ndims(x), dim]);
  if length(shape) < numberOfDimensions
    shape = [shape, ones(1, numberOfDimensions - length(shape))];
  end

  remainingDimensions = 1:numberOfDimensions;
  remainingDimensions(dim) = [];
  permutationOrder = [dim, remainingDimensions];
  permutedShape = shape(permutationOrder);
  xPermuted = permute(x, permutationOrder);
  x2D = reshape(xPermuted, permutedShape(1), []);

  y2D = zeros(permutedShape(1) * factor, size(x2D, 2), class(x));
  y2D(phase + 1:factor:end, :) = x2D;

  outputShape = permutedShape;
  outputShape(1) = size(y2D, 1);
  yPermuted = reshape(y2D, outputShape);
  y = ipermute(yPermuted, permutationOrder);
end
%=============================================================================
