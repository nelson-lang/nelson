%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bodeplot(varargin)
  % Plot the Bode diagram of a SISO LTI system with plotting options.
  %
  % bodeplot(sys)
  % bodeplot(sys, opt)
  % bodeplot(sys, lineSpec)
  % bodeplot(sys, opt, lineSpec)
  % bodeplot(sys, w, ...)
  % [magnitude, phase, w] = bodeplot(...)
  %
  % opt is an options structure as returned by bodeoptions. The honored
  % fields are PhaseUnits ('deg' | 'rad') and Grid ('off' | 'on').

  narginchk(1, 4);
  nargoutchk(0, 3);

  [sys, opt, lineSpec, w] = parseArguments(varargin);

  if ~islti(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end
  if ~issiso(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end

  if isempty(w)
    w = defaultFrequencyRange(sys);
  end
  if isempty(w)
    [magnitude, phase, wout] = bode(sys);
  else
    [magnitude, phase, wout] = bode(sys, w);
  end
  magnitude = reshape(magnitude, 1, []);
  phase = reshape(phase, 1, []);
  wout = reshape(wout, 1, []);

  if (nargout == 0)
    drawBodePlot(wout, magnitude, phase, lineSpec, opt);
  end

  if (nargout > 0)
    varargout{1} = reshape(magnitude, 1, 1, []);
  end
  if (nargout > 1)
    varargout{2} = reshape(phase, 1, 1, []);
  end
  if (nargout > 2)
    varargout{3} = wout;
  end
end
%=============================================================================
function [sys, opt, lineSpec, w] = parseArguments(args)
  sys = args{1};
  opt = bodeoptions();
  lineSpec = '';
  w = [];
  for k = 2:numel(args)
    arg = args{k};
    if (isstruct(arg))
      opt = mergeOptions(opt, arg);
    elseif (ischar(arg) || isStringScalar(arg))
      lineSpec = char(arg);
    else
      w = arg;
    end
  end
  if isempty(lineSpec)
    lineSpec = '-';
  end
end
%=============================================================================
function w = defaultFrequencyRange(sys)
  % Auto-select a frequency grid framing the system dynamics: one decade
  % below the slowest corner frequency up to one decade above the fastest.
  w = [];
  if ~isct(sys) || ~isprop(sys, 'Numerator') || ~isprop(sys, 'Denominator')
    return
  end
  numerator = sys.Numerator{1};
  denominator = sys.Denominator{1};
  corners = abs([roots(numerator(:)); roots(denominator(:))]);
  corners = corners(isfinite(corners) & corners > 0);
  if isempty(corners)
    return
  end
  decadeMin = floor(log10(min(corners))) - 1;
  decadeMax = ceil(log10(max(corners))) + 1;
  w = logspace(decadeMin, decadeMax, 1000);
end
%=============================================================================
function opt = mergeOptions(defaults, provided)
  opt = defaults;
  names = fieldnames(provided);
  for k = 1:numel(names)
    opt.(names{k}) = provided.(names{k});
  end
end
%=============================================================================
function drawBodePlot(w, magnitude, phase, lineSpec, opt)
  usePhaseRadians = isPhaseInRadians(opt);
  if usePhaseRadians
    phaseToPlot = phase * pi / 180;
    phaseLabel = _('Phase (rad)');
  else
    phaseToPlot = phase;
    phaseLabel = _('Phase (deg)');
  end
  useGrid = isGridOn(opt);

  f = figure('Visible', 'off');
  magnitudeAxes = subplot(2, 1, 1);
  semilogx(w, magnitude, lineSpec);
  title(_('Bode Diagram'));
  ylabel(_('Magnitude (dB)'));
  if useGrid
    grid(magnitudeAxes, 'on');
  end

  phaseAxes = subplot(2, 1, 2);
  semilogx(w, phaseToPlot, lineSpec);
  phaseAxes.YLim = phasePlotLimits(phaseToPlot, usePhaseRadians);
  phaseAxes.YTick = phasePlotTicks(phaseAxes.YLim, usePhaseRadians);
  ylabel(phaseLabel);
  xlabel(_('Frequency (rad/s)'));
  if useGrid
    grid(phaseAxes, 'on');
  end

  setBodeAxesLayout(magnitudeAxes, phaseAxes);
  % The magnitude and phase plots share the frequency axis: keep the tick marks
  % on the magnitude plot but show the frequency labels only under the phase
  % plot at the bottom.
  magnitudeAxes.XTickLabel = {};
  f.Visible = 'on';
end
%=============================================================================
function tf = isPhaseInRadians(opt)
  tf = isfield(opt, 'PhaseUnits') && strcmpi(char(opt.PhaseUnits), 'rad');
end
%=============================================================================
function tf = isGridOn(opt)
  tf = isfield(opt, 'Grid') && strcmpi(char(opt.Grid), 'on');
end
%=============================================================================
function lim = phasePlotLimits(phase, usePhaseRadians)
  step = phaseStep(usePhaseRadians);
  lower = step * floor(min(phase) / step);
  upper = step * ceil(max(phase) / step);
  if (lower == upper)
    lower = lower - step;
    upper = upper + step;
  end
  lim = [lower upper];
end
%=============================================================================
function ticks = phasePlotTicks(lim, usePhaseRadians)
  step = phaseStep(usePhaseRadians);
  ticks = lim(1):step:lim(2);
end
%=============================================================================
function step = phaseStep(usePhaseRadians)
  if usePhaseRadians
    step = 0.5;
  else
    step = 45;
  end
end
%=============================================================================
function setBodeAxesLayout(magnitudeAxes, phaseAxes)
  magnitudeAxes.Position = [0.13 0.56 0.775 0.34];
  phaseAxes.Position = [0.13 0.12 0.775 0.34];
end
%=============================================================================
