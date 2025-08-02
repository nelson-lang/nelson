%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = lqry(varargin)
  % Form linear-quadratic (LQ) state-feedback regulator with output weighting
  % [K, S, e] = lqry(A, B, C, D, Q, R)
  % [K, S, e] = lqry(A, B, C, D, Q, R, N)
  narginchk(6, 7);
  nargoutchk(0, 3);
  
  A = varargin{1};
  mustBeNumeric(A, 1);
  B = varargin{2};
  mustBeNumeric(B, 2);
  C = varargin{3};
  mustBeNumeric(C, 3);
  D = varargin{4};
  mustBeNumeric(D, 4);
  Q = varargin{5};
  mustBeNumeric(Q, 5);
  R = varargin{6};
  mustBeNumeric(R, 6);
  if nargin > 6
    N = varargin{7};
  else
    N = zeros(size(Q, 1), size(R, 1));
  end
  mustBeNumeric(N, 7);
  sys = ss(A, B, C, D);
  [K, S, e] = lqry(sys, Q, R, N);
  varargout{1} = K;
  if nargout > 1
    varargout{2} = S;
  end
  if nargout > 2
    varargout{3} = e;
  end
end
