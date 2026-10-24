%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function hsv = rgb2hsv(rgb)
  narginchk(1, 1);
  nargoutchk(0, 1);
  isMap = ip_is_colormap(rgb);
  if isMap
    if isempty(rgb)
      error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Colormap');
    end
    if ~isa(rgb, 'double') || any(rgb(:) < 0) || any(rgb(:) > 1)
      error('Nelson:image_processing:colormapDoubleMatrixValuesRange', 'Colormap must be a double matrix with values in the range [0, 1].');
    end
    rgbMap = reshape(rgb, size(rgb, 1), 1, 3);
    hsvMap = rgb2hsv(rgbMap);
    hsv = reshape(hsvMap, size(rgb, 1), 3);
    return;
  end
  if ~ip_is_rgb_image(rgb)
    error('Nelson:image_processing:rgbImageMNArray', '%s must be an M-by-N-by-3 array.', 'RGB image');
  end
  if ~(isa(rgb, 'double') || isa(rgb, 'single') || isa(rgb, 'uint8') || isa(rgb, 'uint16'))
    error('Nelson:image_processing:rgbImageDoubleSingleUint8Uint16', 'RGB image must be double, single, uint8, or uint16.');
  end
  if ~isreal(rgb)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'RGB image');
  end
  outIsSingle = isa(rgb, 'single');
  if isempty(rgb)
    hsv = zeros(size(rgb));
    if outIsSingle
      hsv = single(hsv);
    end
    return;
  end
  rgb = ip_to_unit_double(rgb);
  r = rgb(:, :, 1);
  g = rgb(:, :, 2);
  b = rgb(:, :, 3);
  v = max(rgb, [], 3);
  v = v(:, :, 1);
  mn = min(rgb, [], 3);
  mn = mn(:, :, 1);
  d = v - mn;
  safeD = d + (d == 0);
  safeV = v + (v == 0);
  hr = mod((g - b) ./ safeD, 6) / 6;
  hg = ((b - r) ./ safeD + 2) / 6;
  hb = ((r - g) ./ safeD + 4) / 6;
  useR = v == r;
  useG = (v ~= r) & (v == g);
  useB = (v ~= r) & (v ~= g) & (v == b);
  h = (d ~= 0) .* (useR .* hr + useG .* hg + useB .* hb);
  s = d ./ safeV;
  hsv = zeros(size(rgb, 1), size(rgb, 2), 3);
  hsv(:, :, 1) = h;
  hsv(:, :, 2) = s;
  hsv(:, :, 3) = v;
  if outIsSingle
    hsv = single(hsv);
  end
end
%=============================================================================
