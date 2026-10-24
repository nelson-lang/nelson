%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rgb = ycbcr2rgb(ycbcr)
  narginchk(1, 1);
  nargoutchk(0, 1);
  isMap = ip_is_colormap(ycbcr);
  if isMap
    if ~isa(ycbcr, 'double') || any(ycbcr(:) < 0) || any(ycbcr(:) > 1)
      error('Nelson:image_processing:colormapDoubleMatrixValuesRange', 'Colormap must be a double matrix with values in the range [0, 1].');
    end
    ycbcrMap = reshape(ycbcr, size(ycbcr, 1), 1, 3);
    rgbMap = ycbcr2rgb(ycbcrMap);
    rgb = reshape(rgbMap, size(ycbcr, 1), 3);
    return;
  end
  if ~ip_is_rgb_image(ycbcr)
    error('Nelson:image_processing:ycbcrImageMNArray', '%s must be an M-by-N-by-3 array.', 'YCbCr image');
  end
  d = ip_to_unit_double(ycbcr);
  y = 255 * d(:, :, 1);
  cb = 255 * d(:, :, 2) - 128;
  cr = 255 * d(:, :, 3) - 128;
  r = 1.164383 * (y - 16) + 1.596027 * cr;
  g = 1.164383 * (y - 16) - 0.391762 * cb - 0.812968 * cr;
  b = 1.164383 * (y - 16) + 2.017232 * cb;
  out = ip_clip01(cat(3, r, g, b) / 255);
  switch class(ycbcr)
    case 'uint8'
      rgb = uint8(round(out * 255));
    case 'uint16'
      rgb = uint16(round(out * 65535));
    case 'single'
      rgb = single(out);
    otherwise
      rgb = out;
  end
end
%=============================================================================
