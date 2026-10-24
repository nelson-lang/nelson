%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = imref3d(varargin)
  narginchk(0, 4);
  nargoutchk(0, 1);
  if nargin == 0
    imageSize = [2 2 2];
  else
    imageSize = varargin{1};
  end
  if nargin == 2 || nargin == 3
    error('Nelson:image_processing:imref3dRequiresImageSizeXY', 'imref3d requires image size only or X, Y and Z world arguments.');
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
  if nargin >= 4
    zWorldLimits = varargin{4};
  else
    zWorldLimits = [];
  end
  ref = ip_make_imref3d(imageSize, xWorldLimits, yWorldLimits, zWorldLimits);
end
%=============================================================================
