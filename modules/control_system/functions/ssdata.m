%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ssdata(varargin)
  % Access state-space model data
  % [a, b, c, d] = ssdata(sys)
  % [a, b, c, d, Ts] = ssdata(sys)
  narginchk(1, 1);
  nargoutchk(0, 5);
  sys = varargin{1};
  
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  sys = ss(sys);
  varargout{1} = sys.A;
  if nargout > 1
    varargout{2} = sys.B;
  end
  if nargout > 2
    varargout{3} = sys.C;
  end
  if nargout > 3
    varargout{4} = sys.D;
  end
  if nargout > 4
    varargout{5} = sys.Ts;
  end
end
