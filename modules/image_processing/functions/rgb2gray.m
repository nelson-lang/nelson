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
  narginchk(1, 1);
  nargoutchk(0, 1);
  if ip_is_colormap(rgb)
    if isempty(rgb)
      error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Colormap');
    end
    if ~isa(rgb, 'double')
      error('Nelson:image_processing:colormapDoubleMatrixValuesRange', 'Colormap must be a double matrix with values in the range [0, 1].');
    end
    grayd = 0.298936021293776 * rgb(:, 1) + 0.587043074451121 * rgb(:, 2) + 0.114020904255103 * rgb(:, 3);
    grayd = ip_clip_unit_result(grayd);
    gray = [grayd grayd grayd];
    return;
  end
  if ~ip_is_rgb_image(rgb)
    error('Nelson:image_processing:rgbImageMNArray', '%s must be an M-by-N-by-3 array.', 'RGB image');
  end
  if isa(rgb, 'logical')
    error('Nelson:image_processing:rgbImageNumericNonLogical', '%s must be numeric and non-logical.', 'RGB image');
  end
  d = ip_to_unit_double(rgb);
  grayd = 0.298936021293776 * d(:, :, 1) + 0.587043074451121 * d(:, :, 2) + 0.114020904255103 * d(:, :, 3);
  grayd = ip_clip_unit_result(grayd);
  switch class(rgb)
    case 'uint8'
      gray = uint8(round(ip_clip01(grayd) * 255));
    case 'uint16'
      gray = uint16(round(ip_clip01(grayd) * 65535));
    case 'uint32'
      gray = uint32(round(ip_clip01(grayd) * 4294967295));
    case 'uint64'
      gray = uint64(round(ip_clip01(grayd) * 18446744073709551615));
    case 'int8'
      gray = int8(round(ip_clip01(grayd) * 255 - 128));
    case 'int16'
      gray = int16(round(ip_clip01(grayd) * 65535 - 32768));
    case 'int32'
      gray = int32(round(ip_clip01(grayd) * 4294967295 - 2147483648));
    case 'int64'
      gray = int64(round(ip_clip01(grayd) * 18446744073709551615 - 9223372036854775808));
    case 'single'
      gray = single(grayd);
    otherwise
      gray = grayd;
  end
end
%=============================================================================
