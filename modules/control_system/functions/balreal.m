%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = balreal(varargin)
  narginchk(3,3);
  nargoutchk(0, 6);
  
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  rD = size(C, 1);
  cD = size(D, 2);
  D = zeros(rD, cD);
  sysIn = ss(A, B, C, D);
  
  [sysOut, g, Ti, T] = balreal(sysIn);
  [abal, bbal, cbal] = ssdata(sysOut);
  
  varargout{1} = abal;
  if nargout > 1
    varargout{2} = bbal;
  end
  if nargout > 2
    varargout{3} = cbal;
  end
  if nargout > 3
    varargout{4} = g;
  end
  if nargout > 4
    varargout{5} = T;
  end
  if nargout > 5
    varargout{6} = Ti;
  end
end