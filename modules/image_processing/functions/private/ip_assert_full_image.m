%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_assert_full_image(img, allowRGB, allowRGBA)
  if nargin < 3
    allowRGBA = false;
  end
  if ~(isnumeric(img) || islogical(img))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Image');
  end
  if issparse(img)
    error('Nelson:image_processing:sparseImagesNotSupported', '%s are not supported.', 'Sparse images');
  end
  s = size(img);
  if length(s) <= 3
    if length(s) < 3 || s(3) == 1
      return
    end
    if allowRGB && (s(3) == 3 || (allowRGBA && s(3) == 4))
      return
    end
  end
  if allowRGB
    if allowRGBA
      error('Nelson:image_processing:imageDRgbRgbaArray', '%s must be a 2-D, RGB, or RGBA array.', 'Image');
    else
      error('Nelson:image_processing:imageDRgbArray', '%s must be a 2-D or RGB array.', 'Image');
    end
  else
    error('Nelson:image_processing:imageDArray', '%s must be a 2-D array.', 'Image');
  end
end
%=============================================================================
