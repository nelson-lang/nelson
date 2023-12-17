%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
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
  
  sys = varargin{1};
  Ts = sys.Ts;
  D = sys.D;
  
  G = gram(sys, 'c');
  [V, S] = svd(G);
  T1 = V * sqrt(S);
  
  A = inv(T1) * sys.A * T1;
  B = inv(T1) * sys.B;
  C = sys.C * T1;
  
  sys = ss(A', C', [], []);
  G = gram(sys, 'c');
  [V, S] = svd(G);
  T2 = V * (S ^ (-1/4));
  
  A = inv(T2) * A * T2;
  B = inv(T2) * B;
  C = C * T2;
  
  sysOut = ss(A, B, C, D);
  
  varargout{1} = sysOut;
  if nargout > 1
    W = gram(ss(sysOut.A, sysOut.B, [], []), 'c');
    G = diag(W);
    varargout{2} = G;
  end
  if nargout > 2
    Ti = T1 * T2;
    T = inv(Ti);
    varargout{3} = T;
  end
  if nargout > 3
    varargout{4} = Ti;
  end
end
