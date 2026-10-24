%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [markerValues, maskValues] = ip_validate_reconstruction_pair(marker, mask)
  ip_assert_full_image(marker, false);
  ip_assert_full_image(mask, false);
  if ~isequal(size(marker), size(mask))
    error('Nelson:image_processing:markerImageSameSizeMaskImage', '%s must have the same size as %s.', 'Marker image', 'the mask image');
  end
  if ~isreal(marker) || ~isreal(mask)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Marker and mask images');
  end
  markerValues = double(marker);
  maskValues = double(mask);
  if any(~isfinite(markerValues(:))) || any(~isfinite(maskValues(:)))
    error('Nelson:image_processing:markerMaskImageValuesFinite', '%s values must be finite.', 'Marker and mask image');
  end
  if any(markerValues(:) > maskValues(:))
    error('Nelson:image_processing:markerImageValuesLessEqualMask', 'Marker image values must be less than or equal to mask image values.');
  end
end
%=============================================================================
