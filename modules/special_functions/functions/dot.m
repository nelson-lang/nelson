%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = dot(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  leftValue = varargin{1};
  rightValue = varargin{2};
  rejectIntegerInput(leftValue, rightValue);
  if islogical(leftValue) || ischar(leftValue)
    leftValue = double(leftValue);
  end
  if islogical(rightValue) || ischar(rightValue)
    rightValue = double(rightValue);
  end

  if nargin == 3
    workingDimension = checkedDimension(varargin{3});
    assertSameShape(leftValue, rightValue);
    varargout{1} = dotAlongDimension(leftValue, rightValue, workingDimension);
  else
    varargout{1} = dotDefaultDimension(leftValue, rightValue);
  end
end
%=============================================================================
function rejectIntegerInput(leftValue, rightValue)
  if isinteger(leftValue) || isinteger(rightValue)
    error('Nelson:dot:invalidType', 'A and B must be single or double.');
  end
end
%=============================================================================
function workingDimension = checkedDimension(workingDimension)
  if ~isnumeric(workingDimension) || ~isscalar(workingDimension)
    error('Nelson:special_functions:invalidDimension', 'Dimension argument must be a positive integer scalar.');
  end
end
%=============================================================================
function assertSameShape(leftValue, rightValue)
  if ~isequal(size(leftValue), size(rightValue))
    error('Nelson:dimagree:aBSize', 'A and B must be same size.');
  end
end
%=============================================================================
function result = dotAlongDimension(leftValue, rightValue, workingDimension)
  if isreal(leftValue) && isreal(rightValue)
    if iscolumn(leftValue) && workingDimension == 1
      result = leftValue' * rightValue;
      return
    end
    if isrow(leftValue) && workingDimension == 2
      result = leftValue * rightValue';
      return
    end
  end
  result = sum(conj(leftValue) .* rightValue, workingDimension);
end
%=============================================================================
function result = dotDefaultDimension(leftValue, rightValue)
  if isvector(leftValue) && isvector(rightValue)
    leftVector = leftValue(:);
    rightVector = rightValue(:);
    if length(leftVector) ~= length(rightVector)
      error('Nelson:dimagree:aBSize', 'A and B must be same size.');
    end
    if isreal(leftVector) && isreal(rightVector)
      result = leftVector' * rightVector;
      return
    end
    result = sum(conj(leftVector) .* rightVector);
    return
  end
  assertSameShape(leftValue, rightValue);
  result = sum(conj(leftValue) .* rightValue);
end
%=============================================================================
