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
function varargout = lqr(varargin)
  % Compute the LQR gain matrix control law L with the weighing matricies Q and R
  % [K, S, e] = lqr(SYS, Q, R, N)
  narginchk(3, 4);
  nargoutchk(0, 3);
  sys = ss(varargin{1});
  Q = varargin{2};
  R = varargin{3};
  if nargin > 3
    N = varargin{4};
    [K, S, e] = lqr(sys, Q, R, N);
  else
    [K, S, e] = lqr(sys, Q, R);
  end
  varargout{1} = K;
  if nargout > 1
    varargout{2} = S;
  end
  if nargout > 2
    varargout{3} = e;
  end
end

