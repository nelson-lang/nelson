%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mpower(varargin)
  narginchk(2, 2)
  nargoutchk(0, 1)
  sysA = ss(varargin{1});
  n = varargin{2};
  mustBeNumeric(n, 2);
  mustBeInteger(n, 2);
  mustBeScalarOrEmpty(n, 2);
  mustBeNonempty(n, 2);
  if (n == 0)
    varargout{1} = ss(1);
    return;
  end
  sys = sysA;
  for k = 2:abs(n)
    sys = sys * sysA;
  end
  if n < 0
    sys = inv(sys);
  end
  varargout{1} = sys;
end
%=============================================================================
