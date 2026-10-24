%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = histogram2(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  ax = histogramAxes(parent);
  nextplot = '';
  if ~isempty(ax)
    nextplot = ax.NextPlot;
  end
  constructorArgs = parseHistogram2Arguments(parent, args);
  h = __histogram2__(constructorArgs{:});
  if ~isempty(ax)
    if strcmp(h.DisplayStyle, 'tile')
      view(ax, 2);
    elseif strcmp(nextplot, 'replaceall') || strcmp(nextplot, 'replace')
      view(ax, 3);
      grid(ax, 'on');
      ax.Box = 'on';
    elseif strcmp(nextplot, 'replacechildren')
      view(ax, 3);
      ax.Box = 'on';
    end
    applyHistogram2AxesState(ax, h);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = newplotTarget(args{1}(1));
    args = args(2:end);
  else
    parent = newplotTarget(graphicsParentProperty(args));
  end
end
%=============================================================================
function constructorArgs = parseHistogram2Arguments(parent, args)
  constructorArgs = {};
  if ~isempty(parent)
    constructorArgs = {'Parent', parent};
  end
  if isempty(args)
    error('Nelson:graphics:dataExpected', 'Data expected.');
  end
  dataConsumed = false;
  if ~isPropertyName(args{1})
    if numel(args) < 2
      error('Nelson:histogram2:xAndYExpected', 'X and Y data expected.');
    end
    x = args{1};
    y = args{2};
    if numel(x) ~= numel(y)
      error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'X', 'Y');
    end
    constructorArgs = [constructorArgs, {'Data', [x(:), y(:)]}];
    args = args(3:end);
    dataConsumed = true;
  end
  if dataConsumed && ~isempty(args) && isnumeric(args{1})
    if numel(args) >= 2 && isnumeric(args{2}) && ~isscalar(args{1}) && ~isscalar(args{2})
      constructorArgs = [constructorArgs, {'XBinEdges', args{1}, 'YBinEdges', args{2}}];
      args = args(3:end);
    else
      binArgument = args{1};
      args = args(2:end);
      if isscalar(binArgument)
        constructorArgs = [constructorArgs, {'NumBins', [binArgument binArgument]}];
      elseif numel(binArgument) == 2
        constructorArgs = [constructorArgs, {'NumBins', reshape(binArgument, 1, 2)}];
      else
        error('Nelson:histogram2:invalidNumBins', 'NumBins must be a scalar or two-element vector.');
      end
    end
  end
  if mod(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  constructorArgs = [constructorArgs, args];
end
%=============================================================================
function tf = isPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  names = {'Data', 'BinCounts', 'NumBins', 'XBinEdges', 'YBinEdges', ...
    'BinWidth', 'BinMethod', 'XBinLimits', 'YBinLimits', 'Normalization', ...
    'FaceColor', 'EdgeColor', 'LineWidth', 'LineStyle', 'FaceLighting', ...
    'BinCountsMode', 'XBinLimitsMode', 'YBinLimitsMode', 'DisplayStyle', ...
    'FaceAlpha', 'EdgeAlpha', 'ShowEmptyBins', 'DisplayName', 'Visible', ...
    'Tag', 'UserData'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
function applyHistogram2AxesState(ax, h)
  if strcmp(ax.XLimMode, 'auto')
    ax.XLim = h.XBinLimits;
  end
  if strcmp(ax.YLimMode, 'auto')
    ax.YLim = h.YBinLimits;
  end
  if strcmp(ax.XTickMode, 'auto')
    ticks = histogram2AxisTicks(h.XBinLimits);
    if ~isempty(ticks)
      ax.XTick = ticks;
    end
  end
  if strcmp(ax.YTickMode, 'auto')
    ticks = histogram2AxisTicks(h.YBinLimits);
    if ~isempty(ticks)
      ax.YTick = ticks;
    end
  end
  if strcmp(h.DisplayStyle, 'tile') || ~strcmp(ax.ZTickMode, 'auto')
    return
  end
  values = h.Values(:);
  values = values(isfinite(values));
  if isempty(values)
    return
  end
  upper = max(values);
  if upper <= 0
    upper = 1;
  end
  ticks = histogram2ValueTicks([0 upper]);
  if ~isempty(ticks)
    ax.ZTick = ticks;
  end
end
%=============================================================================
function ticks = histogram2AxisTicks(limits)
  ticks = [];
  span = diff(limits);
  if ~isfinite(span) || span <= 0
    return
  end
  step = histogram2NiceStep(span / 4);
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  ticks = first:step:last;
  ticks(abs(ticks) < step * 1e-10) = 0;
end
%=============================================================================
function ticks = histogram2ValueTicks(limits)
  ticks = [];
  span = diff(limits);
  if ~isfinite(span) || span <= 0
    return
  end
  step = histogram2NiceStep(span / 4);
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  ticks = first:step:last;
  ticks(abs(ticks) < step * 1e-10) = 0;
  if isempty(ticks) || abs(ticks(1) - limits(1)) > step * 1e-10
    ticks = [limits(1), ticks];
  end
  if abs(ticks(end) - limits(2)) > step * 1e-10
    ticks = [ticks, limits(2)];
  end
end
%=============================================================================
function step = histogram2NiceStep(value)
  if ~isfinite(value) || value <= 0
    step = 1;
    return
  end
  scale = 10 ^ floor(log10(value));
  normalized = value / scale;
  if normalized < sqrt(2)
    nice = 1;
  elseif normalized < sqrt(10)
    nice = 2;
  elseif normalized <= 5.8
    nice = 5;
  else
    nice = 10;
  end
  step = nice * scale;
end
%=============================================================================
function ax = histogramAxes(parent)
  if isempty(parent)
    ax = [];
  elseif isgraphics(parent, 'axes')
    ax = parent;
  else
    ax = ancestor(parent, 'axes');
  end
end
%=============================================================================
