%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mask = ip_validate_imimposemin_mask(mask, imageSize)
  if ~(isnumeric(mask) || islogical(mask))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Marker mask');
  end
  if issparse(mask)
    error('Nelson:image_processing:sparseMarkerMasksNotSupported', '%s are not supported.', 'Sparse marker masks');
  end
  if ~isreal(mask)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Marker mask');
  end
  if ~isequal(size(mask), imageSize)
    error('Nelson:image_processing:markerMaskSameSizeImage', '%s must have the same size as %s.', 'Marker mask', 'the image');
  end
  d = double(mask);
  if any(~isfinite(d(:)))
    error('Nelson:image_processing:markerMaskContainFiniteValues', '%s must contain finite values.', 'Marker mask');
  end
  if any(d(:) ~= 0 & d(:) ~= 1)
    error('Nelson:image_processing:markerMaskContainBinaryValues', '%s must contain binary values.', 'Marker mask');
  end
  mask = ip_to_logical_mask(mask);
end
%=============================================================================
