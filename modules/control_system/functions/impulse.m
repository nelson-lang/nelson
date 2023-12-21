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
function varargout = impulse(varargin)
  % Do a impulse response of a transfer function or a state space model
  % [y,t,x] = impulse(sys)
  % [y,t,x] = impulse(sys,tFinal)
  % [y,t,x] = impulse(sys,[t0,tFinal])    
  % [y,t,x] = impulse(sys,t)
  % impulse(...)
  
  narginchk(1, 2);
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
  
  if nargin > 1
    t = varargin{2};
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
  u = zeros(size(sys.B, 2), length(t)); % Creates 1 1 1 1 1 1 1
  u(:,1) = ones(size(u,1), 1);
  x0 = zeros(size(sys.A, 1), 1); % Assume x0 = [0; 0; 0; ..... ; 0]
  
  % Call lsim!
  if nargout == 0
    lsim(sys, u, t, x0);
    title(_('Impulse response'));
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
