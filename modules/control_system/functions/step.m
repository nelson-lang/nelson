%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end
  if ~isa(sys, 'ss')
    sys = ss(sys);
  end
  t0tFinal = false;
  Ts = sys.Ts;
  if nargin > 1
    t = varargin{2};
    % A scalar is a final time (step(sys, tFinal)) and must be expanded into a
    % 0:Ts:tFinal grid below; only a genuine multi-element vector is used as is
    % or, when it has two elements, as a [t0, tFinal] range.
    if isvector(t) && ~isscalar(t)
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
        t = ltiResponseFinalTime(sys);
        if isempty(t)
          t = 10;
        end
      end
    end
  else
    t = ltiResponseFinalTime(sys);
    if isempty(t)
      t = 10;
    end
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
    ax = gca();
    % Dotted light gray line at the steady-state value, kept behind the response.
    if issiso(sys)
      steady = dcgain(sys);
      if (isscalar(steady) && isfinite(steady))
        xLimits = ax.XLim;
        yLimits = ax.YLim;
        hold(ax, 'on');
        topChildren = ax.Children;
        hSteady = plot(ax, xLimits, [steady, steady], ':', 'Color', [0.8, 0.8, 0.8]);
        ax.XLim = xLimits;
        ax.YLim = yLimits;
        ax.Children = [topChildren; hSteady];
        hold(ax, 'off');
      end
    end
    title(ax, _('Step Response'));
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
