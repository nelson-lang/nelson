%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = boxPlotBase(parent, groups, labels, options)
  target = prepareBoxParent(parent);
  ax = boxAxes(target);
  if isempty(labels)
    labels = defaultLabels(numel(groups));
  end
  h = hggroup(target);
  h.Visible = 'off';
  positions = boxPositions(numel(groups), options);
  widths = boxWidths(groups, options);
  for k = 1:numel(groups)
    drawOneBox(h, positions(k), groups{k}, options, k, widths(k));
  end
  applyBoxAxesState(ax, positions, labels, options);
  h.UserData = struct('ChartType', options.ChartType, 'Labels', {labels}, ...
    'Orientation', options.Orientation, 'Groups', {groups}, 'Positions', positions);
  h.Visible = 'on';
end
%=============================================================================
function target = prepareBoxParent(parent)
  if isempty(parent)
    target = newplot();
  elseif isgraphics(parent, 'axes')
    target = newplot(parent);
  elseif isgraphics(parent, 'hggroup')
    target = parent;
  else
    error('Nelson:graphics:invalidParent', 'Parent must be an axes or hggroup object.');
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
function positions = boxPositions(count, options)
  if isempty(options.Positions)
    positions = 1:count;
  else
    positions = options.Positions;
    if numel(positions) ~= count
      error('Nelson:boxPlotBase:invalidNumberOfPositions', 'Number of positions must match number of groups.');
    end
  end
end
%=============================================================================
function widths = boxWidths(groups, options)
  if isscalar(options.Widths)
    widths = repmat(options.Widths, 1, numel(groups));
  elseif numel(options.Widths) == numel(groups)
    widths = options.Widths;
  else
    error('Nelson:boxPlotBase:invalidNumberOfWidths', 'Number of widths must match number of groups.');
  end
end
%=============================================================================
function drawOneBox(parent, position, values, options, index, width)
  stats = boxStats(values, options.Whisker);
  if isempty(stats)
    return
  end
  if strcmp(options.Orientation, 'horizontal')
    drawHorizontalBox(parent, position, stats, options, index, width);
  else
    drawVerticalBox(parent, position, stats, options, index, width);
  end
end
%=============================================================================
function stats = boxStats(values, whisker)
  values = double(values(:));
  values = values(isfinite(values));
  if isempty(values)
    stats = [];
    return
  end
  values = sort(values);
  q1 = percentile(values, 25);
  med = percentile(values, 50);
  q3 = percentile(values, 75);
  spread = q3 - q1;
  lowFence = q1 - whisker * spread;
  highFence = q3 + whisker * spread;
  inliers = values(values >= lowFence & values <= highFence);
  if isempty(inliers)
    low = values(1);
    high = values(end);
  else
    low = min(inliers);
    high = max(inliers);
  end
  outliers = values(values < low | values > high);
  notch = 1.57 * spread / sqrt(numel(values));
  stats = struct('Q1', q1, 'Median', med, 'Q3', q3, 'Low', low, ...
    'High', high, 'Outliers', outliers, 'NotchLow', med - notch, ...
    'NotchHigh', med + notch);
end
%=============================================================================
function value = percentile(values, pct)
  n = numel(values);
  if n == 1
    value = values(1);
    return
  end
  pos = 1 + (n - 1) * pct / 100;
  lo = floor(pos);
  hi = ceil(pos);
  if lo == hi
    value = values(lo);
  else
    value = values(lo) + (pos - lo) * (values(hi) - values(lo));
  end
end
%=============================================================================
function drawVerticalBox(parent, position, stats, options, index, width)
  style = boxStyle(options, index);
  w = width / 2;
  if strcmp(options.Notch, 'on')
    x = [position - w, position - w, position - w / 4, position - w, ...
      position - w, position + w, position + w, position + w / 4, ...
      position + w, position + w];
    y = [stats.Q1, stats.NotchLow, stats.Median, stats.NotchHigh, ...
      stats.Q3, stats.Q3, stats.NotchHigh, stats.Median, stats.NotchLow, stats.Q1];
  else
    x = [position - w, position - w, position + w, position + w];
    y = [stats.Q1, stats.Q3, stats.Q3, stats.Q1];
  end
  patch('Parent', parent, 'XData', x, 'YData', y, 'FaceColor', style.FaceColor, ...
    'CData', style.CData, 'FaceAlpha', style.FaceAlpha, 'EdgeColor', style.EdgeColor, ...
    'LineWidth', options.LineWidth);
  line(parent, [position - w, position + w], [stats.Median, stats.Median], ...
    'Color', style.MedianColor, 'LineWidth', options.LineWidth);
  line(parent, [position, position], [stats.Q3, stats.High], ...
    'Color', style.WhiskerColor, 'LineStyle', style.WhiskerLineStyle, ...
    'LineWidth', options.LineWidth);
  line(parent, [position, position], [stats.Q1, stats.Low], ...
    'Color', style.WhiskerColor, 'LineStyle', style.WhiskerLineStyle, ...
    'LineWidth', options.LineWidth);
  line(parent, [position - w / 2, position + w / 2], [stats.High, stats.High], ...
    'Color', style.WhiskerColor, 'LineWidth', options.LineWidth);
  line(parent, [position - w / 2, position + w / 2], [stats.Low, stats.Low], ...
    'Color', style.WhiskerColor, 'LineWidth', options.LineWidth);
  drawOutliers(parent, position + zeros(size(stats.Outliers)), stats.Outliers, options, style);
end
%=============================================================================
function drawHorizontalBox(parent, position, stats, options, index, width)
  style = boxStyle(options, index);
  w = width / 2;
  if strcmp(options.Notch, 'on')
    x = [stats.Q1, stats.NotchLow, stats.Median, stats.NotchHigh, stats.Q3, ...
      stats.Q3, stats.NotchHigh, stats.Median, stats.NotchLow, stats.Q1];
    y = [position - w, position - w, position - w / 4, position - w, ...
      position - w, position + w, position + w, position + w / 4, ...
      position + w, position + w];
  else
    x = [stats.Q1, stats.Q3, stats.Q3, stats.Q1];
    y = [position - w, position - w, position + w, position + w];
  end
  patch('Parent', parent, 'XData', x, 'YData', y, 'FaceColor', style.FaceColor, ...
    'CData', style.CData, 'FaceAlpha', style.FaceAlpha, 'EdgeColor', style.EdgeColor, ...
    'LineWidth', options.LineWidth);
  line(parent, [stats.Median, stats.Median], [position - w, position + w], ...
    'Color', style.MedianColor, 'LineWidth', options.LineWidth);
  line(parent, [stats.Q3, stats.High], [position, position], ...
    'Color', style.WhiskerColor, 'LineStyle', style.WhiskerLineStyle, ...
    'LineWidth', options.LineWidth);
  line(parent, [stats.Q1, stats.Low], [position, position], ...
    'Color', style.WhiskerColor, 'LineStyle', style.WhiskerLineStyle, ...
    'LineWidth', options.LineWidth);
  line(parent, [stats.High, stats.High], [position - w / 2, position + w / 2], ...
    'Color', style.WhiskerColor, 'LineWidth', options.LineWidth);
  line(parent, [stats.Low, stats.Low], [position - w / 2, position + w / 2], ...
    'Color', style.WhiskerColor, 'LineWidth', options.LineWidth);
  drawOutliers(parent, stats.Outliers, position + zeros(size(stats.Outliers)), options, style);
end
%=============================================================================
function style = boxStyle(options, index)
  color = boxColor(options, index);
  style.WhiskerLineStyle = options.WhiskerLineStyle;
  if isempty(options.Colors)
    style.EdgeColor = options.EdgeColor;
    style.MedianColor = options.MedianColor;
    style.WhiskerColor = options.WhiskerColor;
    style.OutlierColor = options.OutlierColor;
    style.CData = options.BoxFaceColor;
    if strcmp(options.BoxStyle, 'outline')
      style.FaceColor = 'none';
      style.FaceAlpha = 1;
    else
      style.FaceColor = options.BoxFaceColor;
      style.FaceAlpha = options.BoxFaceAlpha;
    end
  else
    style.EdgeColor = color;
    style.MedianColor = color;
    style.WhiskerColor = color;
    style.OutlierColor = color;
    style.CData = color;
    if strcmp(options.BoxStyle, 'outline')
      style.FaceColor = 'none';
      style.FaceAlpha = 1;
    else
      style.FaceColor = color;
      style.FaceAlpha = options.BoxFaceAlpha;
    end
  end
end
%=============================================================================
function color = boxColor(options, index)
  colors = options.Colors;
  if isempty(colors)
    color = options.BoxFaceColor;
  elseif isnumeric(colors)
    row = mod(index - 1, size(colors, 1)) + 1;
    color = colors(row, :);
  else
    row = mod(index - 1, numel(colors)) + 1;
    color = getColorShortName(colors(row));
  end
end
%=============================================================================
function drawOutliers(parent, x, y, options, style)
  if isempty(x) || strcmp(options.ShowOutliers, 'off')
    return
  end
  line(parent, x, y, 'LineStyle', 'none', 'Marker', options.Symbol, ...
    'Color', style.OutlierColor, 'MarkerEdgeColor', style.OutlierColor);
end
%=============================================================================
function applyBoxAxesState(ax, positions, labels, options)
  if strcmp(options.Orientation, 'horizontal')
    ax.YTick = positions;
    ax.YTickLabel = labels(:);
    if strcmp(ax.YLimMode, 'auto')
      ax.YLim = [min(positions) - 0.5, max(positions) + 0.5];
    end
  else
    ax.XTick = positions;
    ax.XTickLabel = labels(:);
    if strcmp(ax.XLimMode, 'auto')
      ax.XLim = [min(positions) - 0.5, max(positions) + 0.5];
    end
  end
  grid(ax, 'on');
end
%=============================================================================
function ax = boxAxes(parent)
  if isgraphics(parent, 'axes')
    ax = parent;
  else
    ax = ancestor(parent, 'axes');
  end
end
%=============================================================================
