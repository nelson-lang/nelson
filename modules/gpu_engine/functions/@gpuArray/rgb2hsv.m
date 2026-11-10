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
  % float colormaps and images are converted on the device, other inputs on the host
  narginchk(1, 1);
  nargoutchk(0, 1);
  [onDevice, isMap] = deviceColorInput(rgb);
  if ~onDevice
    hsv = __gpuArray_host__('rgb2hsv', rgb);
  elseif isMap
    if gather(any(rgb(:) < 0 | rgb(:) > 1))
      error('Nelson:image_processing:colormapDoubleMatrixValuesRange', ...
        'Colormap must be a double matrix with values in the range [0, 1].');
    end
    hsv = reshape(deviceRgbToHsv(rgb(:, 1), rgb(:, 2), rgb(:, 3)), size(rgb, 1), 3);
  else
    hsv = deviceRgbToHsv(rgb(:, :, 1), rgb(:, :, 2), rgb(:, :, 3));
  end
end
%=============================================================================
function hsv = deviceRgbToHsv(r, g, b)
  % hue from the largest channel, saturation and value from the channel range
  v = max(max(r, g), b);
  d = v - min(min(r, g), b);
  safeD = d + (d == 0);
  useR = v == r;
  useG = ~useR & (v == g);
  useB = ~useR & ~useG & (v == b);
  h = useR .* mod((g - b) ./ safeD, 6) + useG .* ((b - r) ./ safeD + 2) + useB .* ((r - g) ./ safeD + 4);
  h = (d ~= 0) .* h / 6;
  hsv = cat(3, h, d ./ (v + (v == 0)), v);
end
%=============================================================================
