%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function  varargout = cart2pol(varargin)
  narginchk(2, 3);
  nargoutchk(0, 3);
  x = varargin{1};
  y = varargin{2};
  if nargin == 3
    z = varargin{3};
  end
  th = atan2(y, x);
  r = hypot(x, y);
  if nargout > 0
    varargout{1} = th;
  end
  if nargout > 1
    varargout{2} = r;
  end
  if (nargout > 2)
    if (nargin == 3)
      varargout{3} = z;
    else
      error('Nelson:unassignedOutputs', _('Three input arguments expected to return three ouput arguments.'));
    end
  end
end
%=============================================================================


