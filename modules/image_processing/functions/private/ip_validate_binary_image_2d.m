%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = ip_validate_binary_image_2d(img)
  if ~(isnumeric(img) || islogical(img))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Image');
  end
  if issparse(img)
    error('Nelson:image_processing:sparseImagesNotSupported', '%s are not supported.', 'Sparse images');
  end
  if ~isreal(img)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Image');
  end
  if length(size(img)) > 2
    error('Nelson:image_processing:imageDArray', '%s must be a 2-D array.', 'Image');
  end
  values = double(img);
  if any(~isfinite(values(:)))
    error('Nelson:image_processing:imageValuesFinite', '%s values must be finite.', 'Image');
  end
  if any(values(:) ~= 0 & values(:) ~= 1)
    error('Nelson:image_processing:imageContainBinaryValues', '%s must contain binary values.', 'Image');
  end
  bw = ip_to_logical_mask(img);
end
%=============================================================================
