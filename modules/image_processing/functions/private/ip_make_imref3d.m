%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = ip_make_imref3d(imageSize, xWorldLimits, yWorldLimits, zWorldLimits)
  if ~isnumeric(imageSize) || ~isreal(imageSize) || numel(imageSize) < 3
    error('Nelson:image_processing:imageSizeContainLeastThreeDimensions', '%s must contain at least three dimensions.', 'Image size');
  end
  imageSize = double(imageSize(:).');
  imageSize = imageSize(1:3);
  if any(~isfinite(imageSize)) || any(imageSize < 1) || any(imageSize ~= floor(imageSize))
    error('Nelson:image_processing:imageSizeContainPositiveIntegerValues', '%s must contain positive integer values.', 'Image size');
  end
  if isempty(xWorldLimits)
    xWorldLimits = [0.5 imageSize(2) + 0.5];
  end
  if isempty(yWorldLimits)
    yWorldLimits = [0.5 imageSize(1) + 0.5];
  end
  if isempty(zWorldLimits)
    zWorldLimits = [0.5 imageSize(3) + 0.5];
  end
  if isscalar(xWorldLimits) && isscalar(yWorldLimits) && isscalar(zWorldLimits)
    xWorldLimits = ip_world_limits_from_pixel_extent(xWorldLimits, imageSize(2), ...
      'PixelExtentInWorldX');
    yWorldLimits = ip_world_limits_from_pixel_extent(yWorldLimits, imageSize(1), ...
      'PixelExtentInWorldY');
    zWorldLimits = ip_world_limits_from_pixel_extent(zWorldLimits, imageSize(3), ...
      'PixelExtentInWorldZ');
  else
    xWorldLimits = ip_validate_world_limits(xWorldLimits, 'XWorldLimits');
    yWorldLimits = ip_validate_world_limits(yWorldLimits, 'YWorldLimits');
    zWorldLimits = ip_validate_world_limits(zWorldLimits, 'ZWorldLimits');
  end
  pixelExtentX = diff(xWorldLimits) / imageSize(2);
  pixelExtentY = diff(yWorldLimits) / imageSize(1);
  pixelExtentZ = diff(zWorldLimits) / imageSize(3);
  ref = struct('Type', 'imref3d', 'ImageSize', imageSize, ...
    'XWorldLimits', xWorldLimits, 'YWorldLimits', yWorldLimits, ...
    'ZWorldLimits', zWorldLimits, ...
    'PixelExtentInWorldX', pixelExtentX, 'PixelExtentInWorldY', pixelExtentY, ...
    'PixelExtentInWorldZ', pixelExtentZ, ...
    'ImageExtentInWorldX', diff(xWorldLimits), 'ImageExtentInWorldY', diff(yWorldLimits), ...
    'ImageExtentInWorldZ', diff(zWorldLimits), ...
    'XIntrinsicLimits', [0.5 imageSize(2) + 0.5], ...
    'YIntrinsicLimits', [0.5 imageSize(1) + 0.5], ...
    'ZIntrinsicLimits', [0.5 imageSize(3) + 0.5]);
end
%=============================================================================
