%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = shiftdim(varargin)
  narginchk(1, 2);
  nargoutchk(0, 2);
  inputArray = varargin{1};
  if nargin == 1
    result = removeLeadingSingletonDimensions(inputArray);
  else
    result = shiftByRequestedCount(inputArray, varargin{2});
  end

  varargout{1} = result.value;
  if nargout > 1
    varargout{2} = result.count;
  end
end
%=============================================================================
function result = removeLeadingSingletonDimensions(inputArray)
  if isrow(inputArray) && ~isscalar(inputArray)
    result.value = inputArray.';
    result.count = 1;
    return
  end

  if ismatrix(inputArray)
    result.value = inputArray;
    result.count = 0;
    return
  end

  inputShape = size(inputArray);
  shiftCount = find(inputShape ~= 1, 1) - 1;
  if isempty(shiftCount) || shiftCount == 0
    result.value = inputArray;
    result.count = 0;
    return
  end

  permutationOrder = [shiftCount + 1:ndims(inputArray), 1:shiftCount];
  result.value = permute(inputArray, permutationOrder);
  result.count = shiftCount;
end
%=============================================================================
function result = shiftByRequestedCount(inputArray, requestedCount)
  if requestedCount > 0
    activeCount = rem(requestedCount, ndims(inputArray));
  else
    activeCount = requestedCount;
  end

  if isempty(activeCount) || activeCount == 0
    result.value = inputArray;
    result.count = 0;
    return
  end

  if activeCount > 0
    if activeCount == 1 && ismatrix(inputArray)
      result.value = inputArray.';
    else
      result.value = permute(inputArray, [activeCount + 1:ndims(inputArray), 1:activeCount]);
    end
  else
    result.value = reshape(inputArray, negativeShiftShape(size(inputArray), -activeCount));
  end
  result.count = activeCount;
end
%=============================================================================
function targetShape = negativeShiftShape(inputShape, leadingSingletonCount)
  targetShape = [ones(1, leadingSingletonCount), inputShape];
  while numel(targetShape) > 2 && targetShape(end) == 1
    targetShape(end) = [];
  end
end
%=============================================================================
