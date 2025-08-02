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
  % [K, S, e] = lqry(sys, Q, R)
  % [K, S, e] = lqry(sys, Q, R, N)
  narginchk(3, 4);
  nargoutchk(0, 3);
  
  sys = ss(varargin{1});
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
  
  [K, S, e] = lqry(sys, Q, R, N);
  varargout{1} = K;
  if nargout > 1
    varargout{2} = S;
  end
  if nargout > 2
    varargout{3} = e;
  end
end
