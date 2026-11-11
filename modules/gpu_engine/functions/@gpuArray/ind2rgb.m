%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rgb = ind2rgb(x, map)
  % colormap lookup on the device; empty, invalid or negative indices use the host rules
  narginchk(2, 2);
  nargoutchk(0, 1);
  idx = deviceIndices(x, size(map, 1));
  if isempty(idx) || size(map, 2) < 3
    rgb = __gpuArray_host__('ind2rgb', x, map);
    return;
  end
  colors = gpuArray(map(:, 1:3));
  sz = size(x);
  rgb = cat(3, reshape(colors(idx, 1), sz), reshape(colors(idx, 2), sz), reshape(colors(idx, 3), sz));
end
%=============================================================================
function idx = deviceIndices(x, n)
  % colormap rows for each pixel, [] when the host must decide
  idx = [];
  if ~isa(x, 'gpuArray') || isempty(x) || ~isreal(x) || ndims(x) > 2 || n == 0
    return;
  end
  v = x(:);
  if any(strcmp(underlyingType(x), {'single', 'double'}))
    idx = floatIndices(v, n);
  elseif ~gather(any(v < 0))
    idx = min(single(v) + 1, n);
  end
end
%=============================================================================
function idx = floatIndices(v, n)
  % 1-based values clipped to the colormap, NaN takes the last row
  idx = [];
  if gather(any(isfinite(v) & (v ~= floor(v))))
    return;
  end
  nanV = isnan(v);
  idx = min(max(v, 1), n) .* ~nanV + n .* nanV;
end
%=============================================================================
