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
function varargout = step(varargin)
  % Simulate continuous time model of a state space model or transfer function
  % [y, t, x] = step(sys, t)
  % [y, t, x] = step(sys, tFinal) 
  % [y, t, x] = step(sys, [t0,tFinal]))
  
  narginchk(1, 2);
  nargoutchk(0, 3);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  if ~isa(sys, 'ss')
    sys = ss(sys);
  end
  t0tFinal = false;
  Ts = sys.Ts;
  if nargin > 1
    t = varargin{2};
    if isvector(t)
      t0tFinal = true;
      if numel(t) == 2
        t0 = t(1);
        tFinal = t(2);
        if (Ts > 0)
          t = t0:Ts:tFinal;
        else
          t = t0:0.01:tFinal; % Sample time assumed to be 0.01
        end
      end
    else
      mustBeScalarOrEmpty(t, 2);
      mustBeNumeric(t, 2);
      if isempty(t)
        t = 10;
      end
    end
  else
    t = 10;
  end
  if ~t0tFinal
    if (Ts > 0)
      t = 0:Ts:t;
    else
      t = 0:0.01:t; % Sample time assumed to be 0.01
    end
  end
  % Multiple signals...or not!
  u = ones(size(sys.B, 2), length(t)); % Creates 1 1 1 1 1 1 1
  x0 = zeros(size(sys.A, 1), 1); % Assume x0 = [0; 0; 0; ..... ; 0]
  
  % Call lsim!
  if nargout == 0
    lsim(sys, u, t, x0);
    title('Step response');
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
