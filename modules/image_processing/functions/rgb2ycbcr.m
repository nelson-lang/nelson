%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ycbcr = rgb2ycbcr(rgb)
  narginchk(1, 1);
  nargoutchk(0, 1);
  isMap = ip_is_colormap(rgb);
  if isMap
    if ~isa(rgb, 'double') || any(rgb(:) < 0) || any(rgb(:) > 1)
      error('Nelson:image_processing:colormapDoubleMatrixValuesRange', 'Colormap must be a double matrix with values in the range [0, 1].');
    end
    rgbMap = reshape(rgb, size(rgb, 1), 1, 3);
    ycbcrMap = rgb2ycbcr(rgbMap);
    ycbcr = reshape(ycbcrMap, size(rgb, 1), 3);
    return;
  end
  if ~ip_is_rgb_image(rgb)
    error('Nelson:image_processing:rgbImageMNArray', '%s must be an M-by-N-by-3 array.', 'RGB image');
  end
  d = ip_clip01(ip_to_unit_double(rgb));
  r = d(:, :, 1); g = d(:, :, 2); b = d(:, :, 3);
  y = 16 / 255 + (65.481 * r + 128.553 * g + 24.966 * b) / 255;
  cb = 128 / 255 + (-37.797 * r - 74.203 * g + 112.0 * b) / 255;
  cr = 128 / 255 + (112.0 * r - 93.786 * g - 18.214 * b) / 255;
  out = cat(3, y, cb, cr);
  switch class(rgb)
    case 'uint8'
      ycbcr = uint8(round(ip_clip01(out) * 255));
    case 'uint16'
      ycbcr = uint16(round(ip_clip01(out) * 65535));
    case 'single'
      ycbcr = single(out);
    otherwise
      ycbcr = out;
  end
end
%=============================================================================
