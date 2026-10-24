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
function varargout = lsim(varargin)
  % Plot simulated time response of dynamic system to arbitrary inputs; simulated response data
  % [Y, T, X] = lsim(SYS, U)
  % [Y, T, X] = lsim(SYS, U, T)
  % [Y, T, X] = lsim(SYS, U, T, X0)

  narginchk(2, 4);
  nargoutchk(0, 3);

  sys = varargin{1};
  if ~islti(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
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

  if isct(sys) % Time continuous
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
      Y = (Cd * x + Dd * U(l, :).').';
    else
      Y(l, :) = (Cd * x + Dd * U(l, :).').';
    end
    q = U(l, :).';
    x = (Ad * x) + (Bd * q);
  end

  % This is for the sub plot - How many max rows should we have
  if nargout == 0
    varargout = {};
    f = gcf();
    outputColor = [0.066 0.443 0.745];
    inputColor = [0.4902 0.4902 0.4902];
    inputStyle = '--';
    if size(Y, 2) == 1
      h = plot(T, Y, 'Color', outputColor, 'LineWidth', 0.5);
      ax = gca();
      title(ax, _('Linear Simulation Results'));
      xlabel(ax, sprintf(_('Time (%s)'), sys.TimeUnit));
      ylabel(ax, _('Amplitude'));
      % Keep the zero baseline in view instead of over-zooming on the data.
      yl = ax.YLim;
      ax.YLim = [min(0, yl(1)), max(0, yl(2))];
      ax.XLim = [T(1), T(end)];
      ax.XTick = lsimTimeTicks(T(1), T(end));
    else
      rows = max(size(Cd, 1), size(Bd, 2));
      nOut = size(C, 1);
      % Shared time axis: identical x-limits and tick positions on every
      % subplot so the stacked panels line up column for column.
      xTicks = lsimTimeTicks(T(1), T(end));
      for i = 1:nOut
        ax = subplot(rows, 1, i);
        minmax = [min(Y(:, i)), max(Y(:, i))];
        plot(ax, T, Y(:, i), 'Color', outputColor, 'LineWidth', 0.5);
        hold(ax, 'on');
        for k = 1:size(B, 2)
          plot(ax, T, U(:, k), 'Color', inputColor, 'LineStyle', inputStyle, 'LineWidth', 0.5);
          minmax = [minmax, min(U(:, k)), max(U(:, k))];
        end
        hold(ax, 'off');
        ax.YLim = [min(minmax), max(minmax)];
        ax.XLim = [T(1), T(end)];
        ax.XTick = xTicks;
        % Per-channel label; the shared 'Amplitude' label sits further left.
        ylabel(ax, sprintf(_('To: Out(%d)'), i));
        if (i == 1)
          title(ax, _('Linear Simulation Results'));
        end
        % The panels share the time axis: keep the tick marks on every
        % subplot but show the number labels and the time label only under
        % the bottom one.
        if (i == nOut)
          xlabel(ax, sprintf(_('Time (%s)'), sys.TimeUnit));
        else
          ax.XTickLabel = {};
        end
      end
      % Shared 'Amplitude' label on a full-figure invisible overlay, centered
      % vertically to the left of the per-channel 'To: Out(i)' labels.
      overlay = axes('Position', [0, 0, 1, 1]);
      overlay.Visible = 'off';
      overlay.XLim = [0, 1];
      overlay.YLim = [0, 1];
      text(overlay, 0.045, 0.5, _('Amplitude'), 'Rotation', 90, ...
        'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle');
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
%=============================================================================
function ticks = lsimTimeTicks(lo, hi)
  % Pick a {1,2,5}x10^k tick step giving at most about six intervals across the
  % time span, matching the reference spacing (0 2 4 6 8 10 over [0, 10]) rather
  % than the denser generic auto-ticks (0 1 2 ... 10).
  range = hi - lo;
  if ~isfinite(range) || range <= 0
    ticks = [lo, hi];
    return
  end
  maxIntervals = 6;
  rawStep = range / maxIntervals;
  magnitude = 10 ^ floor(log10(rawStep));
  step = magnitude;
  for m = [1 2 5 10]
    step = m * magnitude;
    if (range / step) <= maxIntervals
      break
    end
  end
  first = ceil(lo / step) * step;
  last = floor(hi / step) * step;
  ticks = first:step:last;
  ticks(abs(ticks) < step * 1e-9) = 0;
end
%=============================================================================
