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
function varargout = lqe(varargin)
  % Compute the LQE kalman gain matrix with the weighing matricies Q and R and state space model
  % [l, p, e] = lqe(A, G, C, Q, R, N)
  narginchk(3, 6);
  nargoutchk(0, 3);
  
  A = varargin{1};
  G = varargin{2};
  C = varargin{3};
  if nargin > 3
    Q = varargin{4};
  else
    Q = [];
  end
  if nargin > 4
    R = varargin{5};
  else
    R = [];
  end
  if nargin > 5
    N = varargin{6};
  else
    N = [];
  end
  
  if isempty(G)
    [l, p, e] = lqr (A.', C.', Q, R, N);
  elseif (size(G, 2) ~= size(Q, 1)) || (size(Q, 1) ~= size(Q, 2))
    error(_('Wrong size for input argument #2 and #4.'));
  elseif isempty(N)
    [l, p, e] = lqr(A.', C.', G * Q * G .', R);
  elseif (size(G,2) ~= size(N, 1))
    error(_('Wrong size for input argument #2 and #5.'));
  else
    [l, p, e] = lqr(A.', C.', G * Q * G .', R, G * N);
  end
  l = l.';
  varargout{1} = l;
  if nargout > 1
    varargout{2} = p;
  end  
  if nargout > 2
    varargout{3} = e;
  end  
end
