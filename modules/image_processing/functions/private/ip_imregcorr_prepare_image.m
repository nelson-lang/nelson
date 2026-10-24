%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function img = ip_imregcorr_prepare_image(img)
  if ~(isnumeric(img) || islogical(img))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Image');
  end
  if issparse(img)
    error('Nelson:image_processing:sparseImagesNotSupported', '%s are not supported.', 'Sparse images');
  end
  if ~isreal(img)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Image');
  end
  s = size(img);
  if length(s) == 3 && s(3) == 3
    img = rgb2gray(img);
  elseif length(s) > 2
    error('Nelson:image_processing:imageDRgbArray', '%s must be a 2-D or RGB array.', 'Image');
  end
  img = ip_to_unit_double(img);
  if any(~isfinite(img(:)))
    error('Nelson:image_processing:imageValuesFinite', '%s values must be finite.', 'Image');
  end
end
%=============================================================================
