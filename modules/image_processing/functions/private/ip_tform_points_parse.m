%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [T, dim, isProjective] = ip_tform_points_parse(tform)
  if ~isstruct(tform) || ~isscalar(tform) || ~isfield(tform, 'T') ...
      || ~isfield(tform, 'Dimensionality')
    error('Nelson:image_processing:firstArgumentGeometricTransformationStructure', 'First argument must be a geometric transformation structure.');
  end
  T = tform.T;
  if ~isnumeric(T) || ~isreal(T)
    error('Nelson:image_processing:transformationMatrixNumericReal', 'Transformation matrix must be numeric and real.');
  end
  T = double(T);
  dim = double(tform.Dimensionality);
  if ~isequal(size(T), [dim + 1, dim + 1])
    error('Nelson:image_processing:transformationMatrixSizeNotConsistentDimensionality', 'Transformation matrix size is not consistent with its dimensionality.');
  end
  if any(~isfinite(T(:))) || abs(det(T)) <= eps
    error('Nelson:image_processing:transformationMatrixFiniteNonsingular', 'Transformation matrix must be finite and nonsingular.');
  end
  isProjective = isfield(tform, 'Type') && strcmp(tform.Type, 'projective');
end
%=============================================================================
