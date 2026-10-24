%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pzmap(varargin)
  % Pole-zero map of a dynamic system model.
  % pzmap(sys)
  % [p, z] = pzmap(sys)

  narginchk(1, 1);
  nargoutchk(0, 2);

  sys = varargin{1};
  if ~islti(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end
  if ~issiso(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end

  p = pole(sys);
  z = zero(sys);

  if (nargout == 0)
    f = figure('Visible', 'off');
    pzmapPlot(p, z, sys.Ts > 0, sys.Ts);
    f.Visible = 'on';
    return
  end

  varargout{1} = p;
  if (nargout > 1)
    varargout{2} = z;
  end
end
%=============================================================================
function pzmapPlot(p, z, isDiscrete, Ts)
  c = [0 0 1];
  ax = gca();
  cla(ax);
  hold(ax, 'on');
  plot(ax, real(p), imag(p), 'x', 'Color', c, 'LineWidth', 1.5, 'MarkerSize', 8);
  plot(ax, real(z), imag(z), 'o', 'Color', c, 'LineWidth', 1.5, 'MarkerSize', 8);
  [xLimits, yLimits] = pzmapLimits(p, z, isDiscrete);
  ax.XLim = xLimits;
  ax.YLim = yLimits;
  % Origin reference crosshair (dotted light gray lines through Real = 0 and Imaginary = 0)
  gridColor = [0.8, 0.8, 0.8];
  responseChildren = ax.Children;
  hHorizontal = plot(ax, xLimits, [0, 0], ':', 'Color', gridColor);
  hVertical = plot(ax, [0, 0], yLimits, ':', 'Color', gridColor);
  backgroundChildren = [hVertical; hHorizontal];
  if (isDiscrete && Ts > 0)
    theta = linspace(0, 2 * pi, 200);
    hUnitCircle = plot(ax, cos(theta), sin(theta), ':', 'Color', gridColor);
    backgroundChildren = [backgroundChildren; hUnitCircle];
  end
  ax.XLim = xLimits;
  ax.YLim = yLimits;
  % keep the reference lines behind the pole/zero markers
  ax.Children = [responseChildren; backgroundChildren];
  grid(ax, 'on');
  box(ax, 'on');
  hold(ax, 'off');
  title(ax, _('Pole-Zero Map'));
  xlabel(ax, _('Real Axis'));
  ylabel(ax, _('Imaginary Axis'));
end
%=============================================================================
function [xLimits, yLimits] = pzmapLimits(p, z, isDiscrete)
  reValues = [real(p(:)); real(z(:))];
  imValues = [imag(p(:)); imag(z(:))];
  if (isDiscrete)
    reValues = [reValues; -1; 1];
    imValues = [imValues; -1; 1];
  end
  if (isempty(reValues))
    reValues = 0;
    imValues = 0;
  end
  xLimits = axisRange(min(reValues), max(reValues));
  yLimits = axisRange(min(imValues), max(imValues));
end
%=============================================================================
function range = axisRange(lo, hi)
  if (lo == hi)
    pad = max(1, abs(lo));
  else
    pad = 0.1 * (hi - lo);
  end
  range = [lo - pad, hi + pad];
end
%=============================================================================
