%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function gray = rgb2gray(rgb)
  % float colormaps and images are converted on the device, other inputs on the host
  narginchk(1, 1);
  nargoutchk(0, 1);
  [onDevice, isMap] = deviceColorInput(rgb);
  if ~onDevice
    gray = __gpuArray_host__('rgb2gray', rgb);
  elseif isMap
    g = luminance(rgb(:, 1), rgb(:, 2), rgb(:, 3));
    gray = [g g g];
  else
    gray = luminance(rgb(:, :, 1), rgb(:, :, 2), rgb(:, :, 3));
  end
end
%=============================================================================
function g = luminance(r, gr, b)
  % weighted sum clipped to [0, 1], NaN gives 0
  g = 0.298936021293776 * r + 0.587043074451121 * gr + 0.114020904255103 * b;
  g = min(max(g, 0), 1);
end
%=============================================================================
