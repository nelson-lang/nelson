%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function gray = im2gray(img)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if isa(img, 'logical')
    error('Nelson:image_processing:imageNumericNonLogical', '%s must be numeric and non-logical.', 'Image');
  end
  if ~isnumeric(img)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Image');
  end
  s = size(img);
  if length(s) > 2 && ~ip_is_rgb_image(img)
    error('Nelson:image_processing:rgbImageMNArray', '%s must be an M-by-N-by-3 array.', 'RGB image');
  end
  if ip_is_rgb_image(img)
    gray = rgb2gray(img);
  else
    gray = img;
  end
end
%=============================================================================
