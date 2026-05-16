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
  if ~isnumeric(Z) || ~ismatrix(Z) || size(Z, 1) < 2 || size(Z, 2) < 2
    error(_('Z must be a numeric matrix with at least two rows and two columns.'));
  end
  finiteZ = Z(isfinite(Z));
  if isempty(finiteZ) || numel(unique(finiteZ(:))) < 2
    error(_('Z must contain at least two different finite values.'));
  end
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
    error([name, _(' must be numeric.')]);
  end
  if isvector(C)
    if numel(C) ~= vectorLength
      error([name, _(' vector length does not match Z.')]);
    end
    values = double(C(:));
    delta = diff(values);
    if ~(all(delta > 0) || all(delta < 0))
      error([name, _(' vector values must be strictly monotonic.')]);
    end
  elseif ~isequal(size(C), matrixSize)
    error([name, _(' matrix size must match Z.')]);
  end
end
%=============================================================================
