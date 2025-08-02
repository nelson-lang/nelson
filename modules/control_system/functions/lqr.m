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
function varargout = lqr(varargin)
  % Compute the LQR gain matrix control law L with the weighing matricies Q and R
  % [K, S, e] = LQR(A, B, Q, R, N)
  
  narginchk(4, 6);
  nargoutchk(0, 3);
  
  A = varargin{1};
  B = varargin{2};
  Q = varargin{3};
  R = varargin{4};
  
  msg = abcdchk(A, B);
  if ~isempty(msg)
    error(msg.id, msg.message);
  end
  sys = ss(A, B, [], []);
  if nargin > 4
    N = varargin{5};
  else
    [rB, cB] = size(B);
    N = zeros(rB, cB);
  end
  if nargin > 5
    E = varargin{6};
  else
    [rB, cB] = size(B);
    E = eye(rB, rB);
  end
  [K, S, e] = lqr(sys, Q, R, N, E);
  
  varargout{1} = K;
  if nargout > 1
    varargout{2} = S;
  end
  if nargout > 2
    varargout{3} = e;
  end
end
