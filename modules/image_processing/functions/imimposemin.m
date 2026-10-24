%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imimposemin(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  img = varargin{1};
  mask = varargin{2};
  ip_assert_full_image(img, false);
  if ~isreal(img)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Image');
  end
  values = double(img);
  if any(~isfinite(values(:)))
    error('Nelson:image_processing:imageValuesFinite', '%s values must be finite.', 'Image');
  end
  mask = ip_validate_imimposemin_mask(mask, size(img));
  if nargin == 3
    ip_validate_watershed_connectivity(varargin{3});
  end
  out = ip_impose_minima_values(values, mask, img);
end
%=============================================================================
