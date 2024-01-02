%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = initial(varargin)
  % Simulate a transfer function or state space model with initial state vector x0
  % [y,t,x] = initial(sys,x0)
  % [y,t,x] = initial(sys,x0,Tfinal)
  % [y,t,x] = initial(sys,x0,t)
  % [y,t,x] = initial(sys, x0, [t0, tFinal])
  
  narginchk(2, 3);
  nargoutchk(0, 3);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  if ~isa(sys, 'ss')
    sys = ss(sys);
  end
  
  if ~issiso(sys)
    error(_('SISO LTI model expected.'));
  end
  
  Ts = sys.Ts;
  
  x0 = varargin{2};
  x0 = x0(:); % Turn them into a vector
  if (size(sys.A, 1) ~= size(x0, 1))
    error(_('The initial conditions vector has not the same row length as matrix A.'));
  end
  
  if nargin > 2
    t = varargin{3};
  else
    t = 10;
  end
  
  if isvector(t) && ~isscalar(t) 
    if (numel(t) == 2)
      if (Ts > 0)
        t = t(1):Ts:t(2);
      else
        t = t(1):0.01:t(2); % Sample time assumed to be 0.01
      end
    end
  else
    if (Ts > 0)
      t = 0:Ts:t;
    else
      t = 0:0.01:t; % Sample time assumed to be 0.01
    end
  end
  % Multiple signals...or not!
  u = zeros(size(sys.B, 2), length(t)); % Creates 0 0 0 0 0 0 0
  
  if nargout == 0
    lsim(sys, u, t, x0);
    title(_('Response to initial Conditions'));
  else
    [Y, T, X] = lsim(sys, u, t, x0);
    varargout{1} = Y;
    if nargout > 1
      varargout{2} = T;
    end
    if nargout > 2        
      varargout{3} = X;
    end
  end    
end
