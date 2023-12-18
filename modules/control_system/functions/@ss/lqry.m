%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = lqry(varargin)
  % Form linear-quadratic (LQ) state-feedback regulator with output weighting
  % [K, S, e] = lqry(sys, Q, R)
  % [K, S, e] = lqry(sys, Q, R, N)
  narginchk(3, 4);
  nargoutchk(0, 3);
  
  sys = varargin{1};
  Q = varargin{2};
  R = varargin{3};
  
  mustBeNumeric(Q, 2);
  mustBeNumeric(R, 3);
  if nargin == 4
    N = varargin{4};
  else
    N = zeros(size(Q, 1), size(R, 1));
  end
  mustBeNumeric(R, 4);
  E = sys.E;
  if ~isempty(E) && rcond(E) < eps()
    error(_('Wrong value for argument #1: sys.E must not be singular.'));
  end
  
  rA = size(sys.A, 1);
  if isempty(E)
    E = eye(rA, rA);
  end
  
  nd = N' * sys.D;
  
  qmod = sys.C' * Q * sys.C;
  rmod = R + sys.D' * Q * sys.D + nd + nd';
  nmod = sys.C' * (Q * sys.D + N);
  
  qmod = symmetrize(qmod);
  rmod = symmetrize(rmod);
  [Veig, eigVals] = eig(rmod);
  vr = real(diag(eigVals));
  if min(vr) <= 0
    error(_('Wrong value for input arguments #5 or #6: result must be positive definite.'));
  end
  
  [Veig, eigVals] = eig([qmod, nmod; nmod', rmod]);
  vqnr = real(diag(eigVals));
  
  [K, S, e] = lqr(sys.A, sys.B, qmod, rmod, nmod, sys.E);
  varargout{1} = K;
  if nargout > 1
    varargout{2} = S;
  end
  if nargout > 2
    varargout{3} = e;
  end
end
%=============================================================================
function result = symmetrize(m)
  result = (m + m') / 2;
end
%=============================================================================
