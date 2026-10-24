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
  narginchk(2, 2);
  nargoutchk(0, 1);
  if size(map, 2) < 3
    error('Nelson:image_processing:colormapLeastThreeColumns', 'Colormap must have at least three columns.');
  end
  if isempty(x)
    rgb = zeros(size(x, 1), size(x, 2), 3);
    return;
  end
  idx = double(x);
  if isa(x, 'double') || isa(x, 'single')
    if ~isreal(x)
      error('Nelson:image_processing:indexedImageValuesFiniteIntegers', 'Indexed image values must be finite integers.');
    end
    finiteIdx = isfinite(idx);
    if any(idx(finiteIdx) ~= floor(idx(finiteIdx)))
      error('Nelson:image_processing:indexedImageValuesFiniteIntegers', 'Indexed image values must be finite integers.');
    end
    idx(idx < 1) = 1;
    idx(isnan(idx) | idx > size(map, 1)) = size(map, 1);
  else
    ip_assert_integer_valued(x);
    idx = idx + 1;
    idx(idx > size(map, 1)) = size(map, 1);
  end
  r = reshape(map(idx(:), 1), size(x));
  g = reshape(map(idx(:), 2), size(x));
  b = reshape(map(idx(:), 3), size(x));
  rgb = zeros(size(x, 1), size(x, 2), 3);
  rgb(:, :, 1) = double(r);
  rgb(:, :, 2) = double(g);
  rgb(:, :, 3) = double(b);
end
%=============================================================================
