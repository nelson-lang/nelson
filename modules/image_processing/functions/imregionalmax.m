%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = imregionalmax(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  img = varargin{1};
  ip_assert_full_image(img, false);
  if ~isreal(img)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Image');
  end
  values = double(img);
  if any(~isfinite(values(:)))
    error('Nelson:image_processing:imageValuesFinite', '%s values must be finite.', 'Image');
  end
  conn = 8;
  if nargin == 2
    conn = ip_validate_watershed_connectivity(varargin{2});
  end
  bw = ip_regional_minima_2d(-values, conn);
end
%=============================================================================
