%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tform = fitgeotrans(movingPoints, fixedPoints, transformType)
  narginchk(3, 3);
  nargoutchk(0, 1);
  movingPoints = ip_validate_control_points(movingPoints, 'Moving points');
  fixedPoints = ip_validate_control_points(fixedPoints, 'Fixed points');
  if ~isequal(size(movingPoints), size(fixedPoints))
    error('Nelson:image_processing:movingFixedPointsSameSize', '%s must have the same size.', 'Moving and fixed points');
  end
  if ~(ischar(transformType) || isstring(transformType))
    error('Nelson:image_processing:transformationTypeString', '%s must be a string.', 'Transformation type');
  end
  transformType = lower(convertStringsToChars(transformType));
  switch transformType
    case 'affine'
      T = ip_fit_affine_transform(movingPoints, fixedPoints);
      tform = affine2d(T);
    case 'projective'
      T = ip_fit_projective_transform(movingPoints, fixedPoints);
      tform = projective2d(T);
    otherwise
      error('Nelson:image_processing:unsupportedGeometricTransformationType', 'Unsupported "%s" option.', 'geometric transformation type');
  end
end
%=============================================================================
