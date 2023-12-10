%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ord2(varargin)
  narginchk(2,2);
  nargoutchk(2, 4);

  wn = varargin{1};
  z = varargin{2};
  if (nargout == 2)
    varargout{1} = 1;
    varargout{2} = [1 2*z*wn wn*wn];
  else
    varargout{1} = [0 1;-wn*wn, -2*z*wn];
    varargout{2} = [0;1];
    varargout{3} = [1 0];
    varargout{4} = 0;
  end
end
%=============================================================================
