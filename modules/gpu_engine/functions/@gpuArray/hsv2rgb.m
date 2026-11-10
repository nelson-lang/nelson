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
  % valid float colormaps and images are converted on the device, other inputs on the host
  narginchk(1, 1);
  nargoutchk(0, 1);
  [onDevice, isMap] = deviceColorInput(hsv);
  if onDevice && isMap
    % out of range colormaps get the host error
    onDevice = ~gather(any(hsv(:) < 0 | hsv(:) > 1));
  elseif onDevice
    % hues outside [0, 1] or NaN give NaN pixels, computed on the host
    h = hsv(:, :, 1);
    onDevice = gather(all(h(:) >= 0 & h(:) <= 1));
  end
  if ~onDevice
    rgb = __gpuArray_host__('hsv2rgb', hsv);
  elseif isMap
    rgb = reshape(deviceHsvToRgb(hsv(:, 1), hsv(:, 2), hsv(:, 3)), size(hsv, 1), 3);
  else
    rgb = deviceHsvToRgb(hsv(:, :, 1), hsv(:, :, 2), hsv(:, :, 3));
  end
end
%=============================================================================
function rgb = deviceHsvToRgb(h, s, v)
  % sector of the hue picks which channel gets the chroma and which the middle value
  hp = mod(h, 1) * 6;
  c = v .* s;
  x = c .* (1 - abs(mod(hp, 2) - 1));
  m = v - c;
  r = c .* (hp < 1) + x .* ((hp >= 1) & (hp < 2)) + x .* ((hp >= 4) & (hp < 5)) + c .* (hp >= 5);
  g = x .* (hp < 1) + c .* ((hp >= 1) & (hp < 3)) + x .* ((hp >= 3) & (hp < 4));
  b = x .* ((hp >= 2) & (hp < 3)) + c .* ((hp >= 3) & (hp < 5)) + x .* (hp >= 5);
  rgb = cat(3, r + m, g + m, b + m);
end
%=============================================================================
