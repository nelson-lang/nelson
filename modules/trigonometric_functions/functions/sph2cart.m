%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function  varargout = sph2cart(varargin)
  narginchk(3, 3);
  nargoutchk(0, 3);
  azimut = varargin{1};
  elevation = varargin{2};
  radius = varargin{3};
  
  z = radius .* sin(elevation);
  radiuscoselevation = radius .* cos(elevation);
  x = radiuscoselevation .* cos(azimut);
  y = radiuscoselevation .* sin(azimut);
  
  if nargout > 0
    varargout{1} = x;
  end
  if nargout > 1
    varargout{2} = y;
  end
  if (nargout > 2)
    varargout{3} = z;
  end
end
%=============================================================================


