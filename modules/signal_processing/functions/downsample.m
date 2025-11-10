%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = downsample(varargin)
  narginchk(2,3);
  nargoutchk(0,1);
  
  inputArray = varargin{1};
  decimationFactor = varargin{2};
  if nargin < 3
    phaseOffset = 0;
  else
    phaseOffset = varargin{3};
  end
  
  mustBeNumeric(decimationFactor);
  mustBeScalarOrEmpty(decimationFactor);
  mustBeFinite(decimationFactor);
  mustBeInteger(decimationFactor);
  mustBePositive(decimationFactor);
  
  mustBeNumeric(phaseOffset);
  mustBeScalarOrEmpty(phaseOffset);
  mustBeFinite(phaseOffset);
  mustBeInteger(phaseOffset);
  mustBeGreaterThanOrEqual(phaseOffset,0);
  mustBeLessThan(phaseOffset,decimationFactor);
  
  decimationFactor = double(decimationFactor);
  phaseOffset = double(phaseOffset);
  
  % Trivial case
  if decimationFactor == 1
    outputArray = inputArray;
    varargout{1} = outputArray;
    return;
  end
  
  % Save original shape of inputArray (possibly N-D)
  originalShape = size(inputArray);
  numDims = max(2, ndims(inputArray)); % ensure at least 2 dims for vectors
  
  % Find the leading non-singleton dimension
  leadingDimension = find(originalShape ~= 1, 1, 'first');
  if isempty(leadingDimension)
    leadingDimension = 1;
  end
  
  % Inline downsampling: permute → reshape to 2-D → select rows → reshape → ipermute
  actualShape = size(inputArray);
  shapeLength = max(numel(originalShape), numel(actualShape));
  % Pad shape vectors to the same length
  paddedOriginalShape = [originalShape, ones(1, shapeLength - numel(originalShape))];
  paddedActualShape = [actualShape, ones(1, shapeLength - numel(actualShape))];

  % Build permutation that brings leadingDimension to the front (without setdiff)
  remainingDims = 1:shapeLength;
  remainingDims(leadingDimension) = [];        % remove the target dimension
  permutationOrder = [leadingDimension, remainingDims];

  % Permute input so leadingDimension is first and reshape to 2-D
  permutedArray = permute(inputArray, permutationOrder);
  targetDimLength = paddedActualShape(leadingDimension);
  matrix2D = reshape(permutedArray, targetDimLength, []);

  % Downsample rows using phaseOffset and decimationFactor
  downsampledMatrix = matrix2D(phaseOffset + 1 : decimationFactor : end, :);

  % Build permuted shape with updated first dimension, reshape and inverse-permute
  permutedShape = paddedActualShape(permutationOrder);
  permutedShape(1) = size(downsampledMatrix, 1);
  reshapedPermutedArray = reshape(downsampledMatrix, permutedShape);
  outputArray = ipermute(reshapedPermutedArray, permutationOrder);
   
  varargout{1} = outputArray;
end
%=============================================================================
