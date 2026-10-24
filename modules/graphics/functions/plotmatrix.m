%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = plotmatrix(varargin)
  narginchk(1, 4);
  nargoutchk(0, 5);
  args = convertStringToCharArgs(varargin);
  [parentAx, x, y, sameData, markerSpec] = parsePlotMatrixInputs(args);
  [nbRows, xCols] = size(x);
  [yRows, yCols] = size(y);
  if nbRows ~= yRows
    error('Nelson:dimagree:xYNumberRows', 'X and Y must have the same number of rows.');
  end
  h = gobjects(yCols, xCols);
  ax = gobjects(yCols, xCols);
  histHandles = gobjects(1, min(xCols, yCols));
  histAxes = gobjects(1, min(xCols, yCols));
  if ~isempty(parentAx)
    newplot(parentAx);
  end
  parentFigure = plotMatrixFigure(parentAx);
  for row = 1:yCols
    for col = 1:xCols
      ax(row, col) = axes('Parent', parentFigure, ...
        'Position', plotMatrixAxesPosition(row, col, yCols, xCols));
      h(row, col) = plot(ax(row, col), x(:, col), y(:, row), markerSpec, 'LineStyle', 'none');
      applyPlotMatrixAxes(ax(row, col), row, col, yCols, xCols);
    end
  end
  if sameData
    [histHandles, histAxes] = drawDiagonalHistograms(ax, x);
  else
    histHandles = gobjects(0);
    histAxes = gobjects(0);
  end
  applyPlotMatrixVariableScales(ax, histAxes, x, y);
  if isempty(parentAx)
    bigax = axes('Position', [0.08 0.08 0.86 0.86], 'Visible', 'off');
  else
    bigax = parentAx;
    bigax.Visible = 'off';
  end
  bigax.Tag = 'PlotMatrixBigAx';
  state = struct();
  state.ChartType = 'plotmatrix';
  state.XData = x;
  state.YData = y;
  state.SameData = sameData;
  state.Axes = ax;
  state.DataHandles = h;
  state.HistogramHandles = histHandles;
  state.HistogramAxes = histAxes;
  bigax.UserData = state;
  outputs = {h, ax, bigax, histHandles, histAxes};
  for k = 1:nargout
    varargout{k} = outputs{k};
  end
end
%=============================================================================
function fig = plotMatrixFigure(parentAx)
  if isempty(parentAx)
    fig = gcf();
  else
    fig = ancestor(parentAx, 'figure');
  end
end
%=============================================================================
function position = plotMatrixAxesPosition(row, col, nbRows, nbCols)
  left = 0.135166666666667;
  right = 0.095;
  bottom = 0.115433333333333;
  top = 0.075;
  horizontalGap = 0.00516666666666667;
  verticalGap = 0.00543333333333333;
  width = (1 - left - right - (nbCols - 1) * horizontalGap) / nbCols;
  height = (1 - bottom - top - (nbRows - 1) * verticalGap) / nbRows;
  x = left + (col - 1) * (width + horizontalGap);
  y = 1 - top - row * height - (row - 1) * verticalGap;
  position = [x y width height];
end
%=============================================================================
function applyPlotMatrixVariableScales(ax, histAxes, x, y)
  xScales = plotMatrixScales(x);
  yScales = plotMatrixScales(y);
  [nbRows, nbCols] = size(ax);
  for row = 1:nbRows
    for col = 1:nbCols
      ax(row, col).XLim = xScales(col).Limits;
      ax(row, col).XTick = xScales(col).Ticks;
      ax(row, col).YLim = yScales(row).Limits;
      ax(row, col).YTick = yScales(row).Ticks;
    end
  end
  n = min(numel(histAxes), numel(xScales));
  for k = 1:n
    histAxes(k).XLim = xScales(k).Limits;
  end
end
%=============================================================================
function scales = plotMatrixScales(values)
  n = size(values, 2);
  scales = repmat(struct('Limits', [0 1], 'Ticks', []), 1, n);
  for k = 1:n
    [limits, ticks] = plotMatrixColumnScale(values(:, k));
    scales(k).Limits = limits;
    scales(k).Ticks = ticks;
  end
end
%=============================================================================
function [limits, ticks] = plotMatrixColumnScale(values)
  values = values(isfinite(values));
  if isempty(values)
    limits = [0 1];
    ticks = [];
    return
  end
  values = values(:).';
  low = min(values);
  high = max(values);
  if low == high
    limits = [low - 0.5, high + 0.5];
    ticks = low;
    return
  end
  if all(abs(values - round(values)) < 1e-12)
    uniqueValues = unique(values);
    if numel(uniqueValues) > 1 && all(abs(diff(uniqueValues) - 1) < 1e-12)
      ticks = low:high;
      limits = [low - 0.6, high + 0.6];
      return
    end
  end
  step = plotMatrixNiceStep((high - low) / 5);
  first = floor(low / step) * step;
  last = ceil(high / step) * step;
  ticks = first:step:last;
  limits = [first - 0.75 * step, last + 0.75 * step];
end
%=============================================================================
function step = plotMatrixNiceStep(value)
  value = abs(value);
  if value <= 0 || ~isfinite(value)
    step = 1;
    return
  end
  scale = 10 ^ floor(log10(value));
  normalized = value / scale;
  if normalized <= 1
    nice = 1;
  elseif normalized <= 2
    nice = 2;
  elseif normalized <= 5
    nice = 5;
  else
    nice = 10;
  end
  step = nice * scale;
end
%=============================================================================
function [parentAx, x, y, sameData, markerSpec] = parsePlotMatrixInputs(args)
  sameData = true;
  markerSpec = '.';
  parentAx = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    parentAx = args{1};
    args = args(2:end);
  end
  if isempty(args) || ~isnumeric(args{1})
    error('Nelson:plotmatrix:numericMatrixExpected', '%s must be a numeric matrix.', 'Value');
  end
  x = double(args{1});
  y = x;
  args = args(2:end);
  if ~isempty(args) && isnumeric(args{1})
    y = double(args{1});
    sameData = false;
    args = args(2:end);
  end
  if ~isempty(args)
    if numel(args) == 1 && (ischar(args{1}) || isStringScalar(args{1}))
      markerSpec = char(args{1});
    else
      error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'plotmatrix');
    end
  end
  if ~ismatrix(x) || ~ismatrix(y)
    error('Nelson:plotmatrix:invalidXAndY', 'X and Y must be matrices.');
  end
end
%=============================================================================
function [histHandles, histAxes] = drawDiagonalHistograms(ax, x)
  n = min(size(ax, 1), size(ax, 2));
  histHandles = gobjects(1, n);
  histAxes = gobjects(1, n);
  for k = 1:n
    pos = ax(k, k).Position;
    histAxes(k) = axes('Position', pos, 'Visible', 'on', 'Tag', 'PlotMatrixHistAx');
    histHandles(k) = histogram(histAxes(k), x(:, k));
    histAxes(k).XTick = [];
    histAxes(k).YTick = [];
    histAxes(k).XGrid = 'off';
    histAxes(k).YGrid = 'off';
  end
end
%=============================================================================
function applyPlotMatrixAxes(ax, row, col, nbRows, nbCols)
  ax.Tag = 'PlotMatrixScatterAx';
  if row < nbRows
    ax.XTickLabel = {};
  end
  if col > 1
    ax.YTickLabel = {};
  end
end
%=============================================================================
