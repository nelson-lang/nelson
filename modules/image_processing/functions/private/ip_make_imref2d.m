%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = ip_make_imref2d(imageSize, xWorldLimits, yWorldLimits)
  if ~isnumeric(imageSize) || ~isreal(imageSize) || numel(imageSize) < 2
    error('Nelson:image_processing:imageSizeContainLeastTwoDimensions', '%s must contain at least two dimensions.', 'Image size');
  end
  imageSize = double(imageSize(:).');
  imageSize = imageSize(1:2);
  if any(~isfinite(imageSize)) || any(imageSize < 1) || any(imageSize ~= floor(imageSize))
    error('Nelson:image_processing:imageSizeContainPositiveIntegerValues', '%s must contain positive integer values.', 'Image size');
  end
  if isempty(xWorldLimits)
    xWorldLimits = [0.5 imageSize(2) + 0.5];
  end
  if isempty(yWorldLimits)
    yWorldLimits = [0.5 imageSize(1) + 0.5];
  end
  if isscalar(xWorldLimits) && isscalar(yWorldLimits)
    xWorldLimits = ip_world_limits_from_pixel_extent(xWorldLimits, imageSize(2), ...
      'PixelExtentInWorldX');
    yWorldLimits = ip_world_limits_from_pixel_extent(yWorldLimits, imageSize(1), ...
      'PixelExtentInWorldY');
  else
    xWorldLimits = ip_validate_world_limits(xWorldLimits, 'XWorldLimits');
    yWorldLimits = ip_validate_world_limits(yWorldLimits, 'YWorldLimits');
  end
  pixelExtentX = diff(xWorldLimits) / imageSize(2);
  pixelExtentY = diff(yWorldLimits) / imageSize(1);
  ref = struct('Type', 'imref2d', 'ImageSize', imageSize, ...
    'XWorldLimits', xWorldLimits, 'YWorldLimits', yWorldLimits, ...
    'PixelExtentInWorldX', pixelExtentX, 'PixelExtentInWorldY', pixelExtentY, ...
    'ImageExtentInWorldX', diff(xWorldLimits), 'ImageExtentInWorldY', diff(yWorldLimits), ...
    'XIntrinsicLimits', [0.5 imageSize(2) + 0.5], ...
    'YIntrinsicLimits', [0.5 imageSize(1) + 0.5]);
end
%=============================================================================
