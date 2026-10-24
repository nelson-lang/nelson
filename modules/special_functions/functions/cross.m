%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function c = cross(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);

  leftValue = varargin{1};
  rightValue = varargin{2};

  workingDimension = [];
  if nargin == 3
    workingDimension = varargin{3};
    if ~isscalar(workingDimension)
      error('Nelson:special_functions:invalidDimension', 'Dimension argument must be a positive integer scalar.');
    end
  end

  vectorsOnlyCase = isvector(leftValue) && isvector(rightValue);
  if ~vectorsOnlyCase
    c = generalCaseCrossProduct(leftValue, rightValue, workingDimension, nargin);
  else
    c = vectorCrossProduct(leftValue, rightValue, workingDimension, nargin);
  end
end
%=============================================================================
function c = vectorCrossProduct(leftValue, rightValue, workingDimension, nRhs)

  isWrongDimRhs3 = (nRhs == 3) && ...
    (size(leftValue, workingDimension) ~= 3 || size(rightValue, workingDimension) ~= 3);
  isWrongDimRhs2 = (nRhs == 2) && (length(leftValue) ~= 3 || length(rightValue) ~= 3);

  if isWrongDimRhs3 || isWrongDimRhs2
    error('Nelson:cross:invalidLength', 'Length must 3 in the dimension in which the cross product expected.');
  end

  c1 = leftValue(2) .* rightValue(3) - leftValue(3) .* rightValue(2);
  c2 = leftValue(3) .* rightValue(1) - leftValue(1) .* rightValue(3);
  c3 = leftValue(1) .* rightValue(2) - leftValue(2) .* rightValue(1);

  allColumns = iscolumn(leftValue) && iscolumn(rightValue);
  if ~allColumns
    c = [c1, c2, c3];
  else
    c = [c1; c2; c3];
  end
end
%=============================================================================
function c = generalCaseCrossProduct(leftValue, rightValue, workingDimension, nRhs)
  activeDimension = workingDimension;
  if ~isequal(size(leftValue), size(rightValue))
    error('Nelson:dimagree:sizeExpected', 'Same size expected.');
  end
  if (nRhs == 3 && size(leftValue, activeDimension) ~= 3)
    error('Nelson:cross:invalidLength', 'Length must 3 in the dimension in which the cross product expected.');
  else
    if (nRhs == 2)
      activeDimension = find(size(leftValue) == 3, 1);
      if isempty(activeDimension)
        error('Nelson:cross:noLength3Dimension', 'At least one dimension of length 3 expected.');
      end
    end
  end
  permutationOrder = [activeDimension:max(length(size(leftValue)), activeDimension), ...
                      1:activeDimension - 1];
  permutedLeft = permute(leftValue, permutationOrder);
  permutedRight = permute(rightValue, permutationOrder);
  c = zeros(size(permutedLeft));
  c([3, 1, 2], :) = permutedLeft([1, 2, 3], :) .* permutedRight([2, 3, 1], :) - ...
    permutedLeft([2, 3, 1], :) .* permutedRight([1, 2, 3], :);
  c = reshape(c, size(permutedLeft));
  c = ipermute(c, permutationOrder);
end
%=============================================================================
