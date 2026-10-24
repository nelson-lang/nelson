%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function points = ip_validate_control_points(points, name)
  if ~isnumeric(points) || ~isreal(points)
    error('Nelson:image_processing:mustBeRealNumeric', '%s must be a real numeric array.', name);
  end
  points = double(points);
  if ~ismatrix(points) || size(points, 2) ~= 2
    error('Nelson:image_processing:mustBeNby2', '%s must be an N-by-2 array.', name);
  end
  if size(points, 1) < 2
    error('Nelson:image_processing:mustHaveTwoPoints', '%s must contain at least two points.', name);
  end
  if any(~isfinite(points(:)))
    error('Nelson:image_processing:mustContainFiniteValues', '%s must contain finite values.', name);
  end
end
%=============================================================================
