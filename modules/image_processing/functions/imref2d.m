%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = imref2d(varargin)
  narginchk(0, 3);
  nargoutchk(0, 1);
  if nargin == 0
    imageSize = [2 2];
  else
    imageSize = varargin{1};
  end
  if nargin == 2
    error('Nelson:image_processing:imref2dRequiresImageSizeBothX', 'imref2d requires image size only or both X and Y world arguments.');
  end
  if nargin >= 2
    xWorldLimits = varargin{2};
  else
    xWorldLimits = [];
  end
  if nargin >= 3
    yWorldLimits = varargin{3};
  else
    yWorldLimits = [];
  end
  ref = ip_make_imref2d(imageSize, xWorldLimits, yWorldLimits);
end
%=============================================================================
