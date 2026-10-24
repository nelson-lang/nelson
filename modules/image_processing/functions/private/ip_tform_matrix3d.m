%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = ip_tform_matrix3d(value)
  if isstruct(value)
    if ~isfield(value, 'T')
      error('Nelson:image_processing:transformationStructureContainTField', 'Transformation structure must contain a T field.');
    end
    value = value.T;
  end
  if ~isnumeric(value) || ~isreal(value)
    error('Nelson:image_processing:transformationMatrixNumericReal', 'Transformation matrix must be numeric and real.');
  end
  T = double(value);
  if size(T, 1) == 3 && size(T, 2) == 4
    T = [T; 0 0 0 1];
  end
  if ~isequal(size(T), [4 4])
    error('Nelson:image_processing:transformationMatrix', '%s must be %d-by-%d or %d-by-%d.', 'Transformation matrix', 4, 4, 3, 4);
  end
  if any(~isfinite(T(:))) || abs(det(T)) <= eps
    error('Nelson:image_processing:transformationMatrixFiniteNonsingular', 'Transformation matrix must be finite and nonsingular.');
  end
  if any(abs(T(:, 4) - [0; 0; 0; 1]) > sqrt(eps))
    error('Nelson:image_processing:affineTransformationMatrixLastColumn', 'Affine transformation matrix must have [0 0 0 1] as its last column.');
  end
end
%=============================================================================
