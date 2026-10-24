%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pareto(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  if isempty(args) || ~isnumeric(args{1})
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  values = double(args{1}(:));
  args = args(2:end);
  labels = [];
  threshold = 0.95;
  if ~isempty(args) && isParetoThresholdValue(args{1})
    threshold = paretoThreshold(args{1});
    args = args(2:end);
  elseif ~isempty(args) && ~isParetoPropertyName(args{1})
    labels = labelsFromValue(args{1});
    args = args(2:end);
  end
  if ~isempty(args) && isnumeric(args{1})
    threshold = paretoThreshold(args{1});
    args = args(2:end);
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'axes')
    ax = newplot(parent);
  else
    ax = ancestor(parent, 'axes');
  end
  [values, labels] = sortedParetoValues(values, labels);
  hBar = bar(ax, values, args{:});
  holdState = ishold(ax);
  hold(ax, 'on');
  cumulative = cumsum(values);
  hLine = plot(ax, 1:numel(values), cumulative, 'Color', [0.8660, 0.3290, 0], ...
    'LineStyle', '-', 'LineWidth', 0.5);
  if ~holdState
    hold(ax, 'off');
  end
  visibleLabels = paretoVisibleLabels(values, threshold);
  ax.XTick = 1:visibleLabels;
  ax.XTickLabel = labels(1:visibleLabels);
  ax.XLim = [0.5, numel(values) + 0.5];
  ax.YLim = paretoYLimits(cumulative);
  ax.YTick = paretoValueTicks(ax.YLim);
  createParetoPercentAxis(ax, cumulative);
  if nargout == 1
    varargout{1} = [hBar; hLine];
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [values, labels] = sortedParetoValues(values, labels)
  values = values(isfinite(values));
  if isempty(values)
    error('Nelson:pareto:invalidData', 'Data must contain at least one finite value.');
  end
  if any(values < 0)
    error('Nelson:pareto:invalidDataValues', 'Data values must be nonnegative.');
  end
  if isempty(labels)
    labels = defaultLabels(numel(values));
  end
  if numel(labels) ~= numel(values)
    error('Nelson:pareto:invalidNumberOfLabels', 'Number of labels must match number of values.');
  end
  [values, order] = sort(values, 'descend');
  labels = labels(order);
end
%=============================================================================
function tf = isParetoThresholdValue(value)
  tf = isnumeric(value) && isscalar(value);
end
%=============================================================================
function threshold = paretoThreshold(value)
  if ~isnumeric(value) || ~isscalar(value) || ~isreal(value) || ~isfinite(value) ...
      || value < 0 || value > 1
    error('Nelson:pareto:invalidThreshold', 'Threshold must be a scalar numeric value between 0 and 1.');
  end
  threshold = double(value);
end
%=============================================================================
function n = paretoVisibleLabels(values, threshold)
  total = sum(values);
  if total == 0
    n = numel(values);
    return
  end
  n = find(cumsum(values) / total >= threshold, 1);
  if isempty(n)
    n = numel(values);
  end
end
%=============================================================================
function limits = paretoYLimits(cumulative)
  if isempty(cumulative) || cumulative(end) == 0
    limits = [0, 1];
  else
    limits = [0, cumulative(end)];
  end
end
%=============================================================================
function ticks = paretoValueTicks(limits)
  lo = limits(1);
  hi = limits(2);
  if ~isfinite(lo) || ~isfinite(hi) || hi <= lo
    ticks = [lo, hi];
    return
  end
  step = paretoNiceStep(hi - lo, 10);
  ticks = lo:step:hi;
  tol = step * 1e-9;
  if isempty(ticks) || ticks(end) < hi - tol
    ticks = [ticks, ticks(end) + step];
  end
end
%=============================================================================
function step = paretoNiceStep(range, maxIntervals)
  raw = range / maxIntervals;
  magnitude = 10 ^ floor(log10(raw));
  normalized = raw / magnitude;
  if normalized <= 1
    multiplier = 1;
  elseif normalized <= 2
    multiplier = 2;
  elseif normalized <= 5
    multiplier = 5;
  else
    multiplier = 10;
  end
  step = multiplier * magnitude;
end
%=============================================================================
function percentAxis = createParetoPercentAxis(ax, cumulative)
  parent = ax.Parent;
  total = cumulative(end);
  percentAxis = axes('Parent', parent, 'Position', ax.Position, ...
    'Color', 'none', 'YAxisLocation', 'right', 'Box', 'off', ...
    'XLim', ax.XLim, 'YLim', ax.YLim, 'XTick', []);
  percentAxis.YTick = ax.YTick;
  percentAxis.YTickLabel = percentTickLabels(percentAxis.YTick, total);
  percentAxis.XColor = ax.XColor;
  percentAxis.YColor = ax.YColor;
  percentAxis.FontName = ax.FontName;
  percentAxis.FontSize = ax.FontSize;
  percentAxis.FontAngle = ax.FontAngle;
  percentAxis.FontWeight = ax.FontWeight;
  if isgraphics(parent, 'figure')
    parent.CurrentAxes = ax;
  end
end
%=============================================================================
function labels = percentTickLabels(ticks, total)
  labels = cell(numel(ticks), 1);
  for k = 1:numel(ticks)
    if total == 0
      percent = 0;
    else
      percent = ticks(k) / total * 100;
    end
    labels{k} = [num2str(round(percent)), '%'];
  end
end
%=============================================================================
function labels = labelsFromValue(value)
  if iscellstr(value)
    labels = value(:);
  elseif isstring(value)
    labels = cellstr(value(:));
  elseif ischar(value)
    labels = cellstr(value);
  elseif isnumeric(value) && isreal(value)
    labels = cellstr(num2str(value(:)));
  else
    error('Nelson:pareto:invalidLabels', 'Labels must be numeric, character, string, or cell string values.');
  end
end
%=============================================================================
function labels = defaultLabels(n)
  labels = cell(n, 1);
  for k = 1:n
    labels{k} = num2str(k);
  end
end
%=============================================================================
function tf = isParetoPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  tf = isValidGraphicsProperty('bar', char(value));
end
%=============================================================================
