%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function limits = ip_world_limits_from_pixel_extent(pixelExtent, imageLength, name)
  if ~(isnumeric(pixelExtent) || islogical(pixelExtent)) || ~isreal(pixelExtent)
    error('Nelson:image_processing:mustBePositiveFinite', '%s must be a positive finite scalar.', name);
  end
  pixelExtent = double(pixelExtent);
  if ~isscalar(pixelExtent) || ~isfinite(pixelExtent) || pixelExtent <= 0
    error('Nelson:image_processing:mustBePositiveFinite', '%s must be a positive finite scalar.', name);
  end
  limits = [0.5 * pixelExtent, (imageLength + 0.5) * pixelExtent];
end
%=============================================================================
