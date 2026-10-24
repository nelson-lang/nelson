%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = activecontour(varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  img = varargin{1};
  mask = varargin{2};
  [iterations, method, smoothFactor, contractionBias] = ip_parse_activecontour_options(varargin);
  if strcmp(method, 'edge') && ip_activecontour_is_color_image(img)
    error('Nelson:image_processing:edgeMethodNotSupportedRgbRgba', 'Edge method is not supported for RGB or RGBA images.');
  end
  image2d = ip_activecontour_prepare_image(img);
  d = ip_to_unit_double(image2d);
  if any(~isfinite(d(:)))
    error('Nelson:image_processing:imageValuesFinite', '%s values must be finite.', 'Image');
  end
  ip_validate_activecontour_mask(mask, size(image2d));
  switch method
    case {'chan-vese', 'edge'}
      mask = ip_to_logical_mask(mask);
      bw = builtin('__activecontour_kernel__', image2d, mask, iterations, method, smoothFactor, contractionBias);
    otherwise
      error('Nelson:image_processing:unsupportedActiveContourMethod', 'Unsupported "%s" option.', 'active contour method');
  end
end
%=============================================================================
