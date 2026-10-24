%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imhmin(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  img = varargin{1};
  h = ip_validate_h_minima_height(varargin{2});
  ip_assert_full_image(img, false);
  if ~isreal(img)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Image');
  end
  values = double(img);
  if any(~isfinite(values(:)))
    error('Nelson:image_processing:imageValuesFinite', '%s values must be finite.', 'Image');
  end
  conn = 8;
  if nargin == 3
    conn = ip_validate_watershed_connectivity(varargin{3});
  end
  reconstructed = ip_reconstruct_by_erosion_2d(values + h, values, conn);
  out = ip_cast_like(reconstructed, img);
end
%=============================================================================
