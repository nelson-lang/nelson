%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateContourData(X, Y, Z)
  if ~(isnumeric(Z) || islogical(Z)) || ~ismatrix(Z) || size(Z, 1) < 2 || size(Z, 2) < 2
    error('Nelson:validateContourData:invalidZ', 'Z must be a numeric matrix with at least two rows and two columns.');
  end
  % Data with no relief draws no contour line, which is not a reason to refuse
  % it: the object is still built and reports an empty LevelList. The plotting
  % functions say so with a warning, see warnContourNotRendered; contourc
  % returns its empty matrix without a word.
  if ~isempty(X)
    validateContourCoordinate(X, size(Z, 2), size(Z), 'X');
  end
  if ~isempty(Y)
    validateContourCoordinate(Y, size(Z, 1), size(Z), 'Y');
  end
end
%=============================================================================
function validateContourCoordinate(C, vectorLength, matrixSize, name)
  if ~isnumeric(C)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', name);
  end
  if isvector(C)
    if numel(C) ~= vectorLength
      error('Nelson:validateContourData:vectorLengthMismatch', '%s vector length does not match Z.', name);
    end
    values = double(C(:));
    delta = diff(values);
    if ~(all(delta > 0) || all(delta < 0))
      error('Nelson:validateContourData:mustBeMonotonic', '%s vector values must be strictly monotonic.', name);
    end
  elseif ~isequal(size(C), matrixSize)
    error('Nelson:validateContourData:sizeMismatch', '%s matrix size must match Z.', name);
  end
end
%=============================================================================
