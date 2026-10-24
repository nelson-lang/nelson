%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rgb = hsv2rgb(hsv)
  narginchk(1, 1);
  nargoutchk(0, 1);
  isMap = ip_is_colormap(hsv);
  if isMap
    if isempty(hsv)
      error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'HSV colormap');
    end
    if ~(isa(hsv, 'double') || isa(hsv, 'single') || islogical(hsv))
      error('Nelson:image_processing:hsvColormapDoubleSingleLogical', '%s must be double, single, or logical.', 'HSV colormap');
    end
    if any(double(hsv(:)) < 0) || any(double(hsv(:)) > 1)
      error('Nelson:image_processing:hsvColormapValuesRange', 'HSV colormap values must be in the range [0, 1].');
    end
    hsvMap = reshape(hsv, size(hsv, 1), 1, 3);
    rgbMap = hsv2rgb(hsvMap);
    rgb = reshape(rgbMap, size(hsv, 1), 3);
    return;
  end
  if ~ip_is_rgb_image(hsv)
    error('Nelson:image_processing:hsvImageMNArray', '%s must be an M-by-N-by-3 array.', 'HSV image');
  end
  if ~(isa(hsv, 'double') || isa(hsv, 'single') || islogical(hsv))
    error('Nelson:image_processing:hsvImageDoubleSingleLogical', '%s must be double, single, or logical.', 'HSV image');
  end
  if ~isreal(hsv)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'HSV image');
  end
  outIsSingle = isa(hsv, 'single');
  if isempty(hsv)
    rgb = zeros(size(hsv));
    if outIsSingle
      rgb = single(rgb);
    end
    return;
  end
  hRaw = double(hsv(:, :, 1));
  invalidHue = ~isfinite(hRaw) | hRaw < 0 | hRaw > 1;
  h = mod(hRaw, 1);
  s = double(hsv(:, :, 2));
  v = double(hsv(:, :, 3));
  hp = h * 6;
  c = v .* s;
  xx = c .* (1 - abs(mod(hp, 2) - 1));
  m = v - c;
  r = c .* (hp < 1) + xx .* ((hp >= 1) & (hp < 2)) + xx .* ((hp >= 4) & (hp < 5)) + c .* (hp >= 5);
  g = xx .* (hp < 1) + c .* ((hp >= 1) & (hp < 3)) + xx .* ((hp >= 3) & (hp < 4));
  b = xx .* ((hp >= 2) & (hp < 3)) + c .* ((hp >= 3) & (hp < 5)) + xx .* (hp >= 5);
  rgb = zeros(size(hsv, 1), size(hsv, 2), 3);
  rgb(:, :, 1) = r + m;
  rgb(:, :, 2) = g + m;
  rgb(:, :, 3) = b + m;
  if any(invalidHue(:))
    for c = 1:3
      plane = rgb(:, :, c);
      plane(invalidHue) = NaN;
      rgb(:, :, c) = plane;
    end
  end
  if outIsSingle
    rgb = single(rgb);
  end
end
%=============================================================================
