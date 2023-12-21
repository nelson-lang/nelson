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
function varargout = lsim(varargin)
  % Plot simulated time response of dynamic system to arbitrary inputs; simulated response data
  % [Y, T, X] = lsim(SYS, U)
  % [Y, T, X] = lsim(SYS, U, T)
  % [Y, T, X] = lsim(SYS, U, T, X0)
  
  narginchk(2, 4);
  nargoutchk(0, 3);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  if ~isa(sys, 'ss')
    sys = ss(sys);
  end
  [A, B, C, D, Ts] = ssdata(sys);
  
  U = varargin{2};
  mustBeNumeric(U, 2);
  if isvector(U)
    U = reshape(U, numel(U), 1);
  end
  if nargin > 2
    T = varargin{3};
    mustBeNumeric(T, 3);
    mustBeVector(T, 3);
    mustBeNonempty(T, 3);
  end
  n = size(sys.A, 1);
  if nargin > 3
    x = varargin{4};
    mustBeNumeric(x, 4);
    mustBeVector(x, 4);
  else
    x = zeros(n, 1);
  end
  x = reshape(x, numel(x), 1);
  
  if isct(sys) % Time continous
    sysd = c2d(sys, T(2) - T(1)); % Sample time is the difference T(2) - T(1)
  else
    sysd = sys;
  end   
  
  T = reshape(T, numel(T), 1);
  
  X = zeros(length(T), n);
  Y = [];
  [Ad, Bd, Cd, Dd] = ssdata(sysd);
  % Simulation
  for l = 1:length(T)
    X(l, :) = x;
    if l == 1
      Y = (Cd * x  +  Dd * U(l, :).').';
    else
      Y(l, :) = (Cd * x  +  Dd * U(l, :).').';
    end
    q = U(l, :).';
    x = (Ad * x)  +  (Bd * q);
  end
  
  % If we have a sample time bigger that 0, then we need to make sure
  % that the signal look like it's in discrete form.
  % If the model has the sample time t(2) - t(1)
  % the model will be inteprented as time continous
  if (Ts > 0)
    % Change t, u and y vector so the plot look like it is discrete
    for(i = 1:2:length(y)*2)
      leftPart = y(:, 1:i);
      rightPart = y(:, (i+1):end);
      y = [leftPart, y(:, i), rightPart];
    end
    for(i = 1:2:length(U)*2)
      leftPart = U(:, 1:i);
      rightPart = U(:, (i+1):end);
      U = [leftPart, U(:, i), rightPart];
    end
    for(i = 1:2:length(T)*2)
      leftPart = T(1:i);
      rightPart = T((i+1):end);
      T = [leftPart, T(i), rightPart];
    end

    % Just remove the first one 
    T = T(:, 2:length(T));
    % And the last one
    Y = Y(:, 1:(length(Y)-1));
    u = u(:, 1:(length(U)-1));
    % Now we have three vectors which look like a discrete signal
  end

  % This is for the sub plot - How many max rows should we have
  if nargout == 0
    varargout = {};
    f = gcf();
    if size(Y, 2) == 1
      h = plot(T, Y);
      title(_('Linear Simulation Results'));
      xlabel(sprintf(_('Time (%s)'), sys.TimeUnit));
      ylabel(_('Amplitude'));
    else
      rows = max(size(Cd,1), size(Bd, 2));
      title(_('Linear Simulation Results'));
      hold on
      for i = 1:size(C, 1)
        ax = subplot(rows, 1, i);
        minmax = [min(Y(:, i)), max(Y(:, i))];
        plot(ax, T, Y(:, i));
        xl = xlabel(sprintf(_('Time (%s)'), sys.TimeUnit));
        for k = 1:size(B, 2)
          plot(ax, T, U(:, k));
          minmax = [minmax, min(U(:, k)), max(U(:, k))];
        end
        ax.YLim = [min(minmax), max(minmax)];
      end
      hold off
      ax = axes();
      ax.Visible = 'off';
      title(_('Linear Simulation Results'));
      yl = ylabel(_('Amplitude'));
      yl.Visible = 'on';
    end
  else
    varargout{1} = Y;
    if nargout > 1
      varargout{2} = T;
    end
    if nargout > 2        
      varargout{3} = X;
    end
  end
end
