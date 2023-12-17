%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = balreal(varargin)
  narginchk(1, 1);
  nargoutchk(0, 4);
  sys = ss(varargin{1});
  [sysb, g, T, Ti] = balreal(sys);
  varargout{1} = sysb;
  if nargout > 1
    varargout{2} = g;
  end
  if nargout > 2
    varargout{3} = T;
  end
  if nargout > 3
    varargout{4} = Ti;
  end
end
