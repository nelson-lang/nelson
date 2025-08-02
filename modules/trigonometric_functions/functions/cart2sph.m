%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function  varargout = cart2sph(varargin)
  narginchk(3, 3);
  nargoutchk(0, 3);
  x = varargin{1};
  y = varargin{2};
  z = varargin{3};
  
  hypotxy = hypot(x, y);
  r = hypot(hypotxy, z);
  elev = atan2(z, hypotxy);
  az = atan2(y, x);
  
  if nargout > 0
    varargout{1} = az;
  end
  if nargout > 1
    varargout{2} = elev;
  end
  if (nargout > 2)
    varargout{3} = r;
  end
end
%=============================================================================


