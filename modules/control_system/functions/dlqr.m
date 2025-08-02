%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = dlqr(varargin)
  % Linear-quadratic regulator design for discrete-time systems.
  % [K, S, e] = dlqr(A, B, Q, R, N)
  % [K, S, e] = dlqr(A, B, Q, R)
  
  narginchk(4,5);
  nargoutchk(0, 3);
  
  A = varargin{1};
  B = varargin{2};
  Q = varargin{3};
  R = varargin{4};
  
  mustBeNumeric(A, 1);
  mustBeNumeric(B, 2);
  
  msg = abcdchk(A, B);
  if ~isempty(msg)
    error(msg.id, msg.message);
  end
  
  sys = ss(A, B, [], [], 1);
  if nargin == 4
    [K, S, CLP] = lqr(sys, Q, R);
  else
    N = varargin{5};
    [K, S, CLP] = lqr(sys, Q, R, N);
  end 
  varargout{1} = K;
  if nargout > 1
    varargout{2} = S;
  end
  if nargout > 2
    varargout{3} = CLP;
  end
end