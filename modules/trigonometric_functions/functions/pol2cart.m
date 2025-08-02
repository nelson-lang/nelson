%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function  varargout = pol2cart(varargin)
  narginchk(2, 3);
  nargoutchk(0, 3);
  th = varargin{1};
  r = varargin{2};
  if nargin == 3
    z = varargin{3};
  end
  x = r .* cos(th);
  y = r .* sin(th);
  if nargout > 0
    varargout{1} = x;
  end
  if nargout > 1
    varargout{2} = y;
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


