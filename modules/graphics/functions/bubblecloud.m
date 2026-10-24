%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bubblecloud(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [chartParent, args] = parseCloudParent(args);
  [sizes, labels, groups, groupLevels, source, args] = parseBubbleCloudData(args);
  opts = parseBubbleCloudOptions(args);
  if isempty(opts.LegendTitle) && ~isempty(source.GroupVariable)
    opts.LegendTitle = source.GroupVariable;
  end
  chartParent = cloudParent(chartParent);
  h = __bubblecloud__('Parent', chartParent, 'SizeData', sizes(:).', ...
    'LabelData', labels(:).', 'GroupData', groups(:).', ...
    'Title', opts.Title, 'LegendTitle', opts.LegendTitle, ...
    'FaceColor', opts.FaceColor, 'EdgeColor', opts.EdgeColor, ...
    'SourceTable', source.Table, 'SizeVariable', source.SizeVariable, ...
    'LabelVariable', source.LabelVariable, 'GroupVariable', source.GroupVariable);
  ax = axes('Parent', chartParent);
  [x, y, radii, bubbleHandles] = drawBubbleCloud(ax, sizes, labels, groups, groupLevels, opts);
  title(ax, opts.Title);
  axis(ax, 'equal');
  updateBubbleCloudAxes(ax, x, y, radii);
  updateBubbleCloudLegend(ax, bubbleHandles, groups, groupLevels, opts);
  set(h, 'Children', ax);
  removeBubbleCloudAxesFromFigure(chartParent, ax);
  try
    chartParent.CurrentAxes = h;
  catch
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function removeBubbleCloudAxesFromFigure(parent, ax)
  if isempty(parent) || ~isgraphics(parent, 'figure')
    return
  end
  children = get(parent, 'Children');
  keptChildren = [];
  for k = 1:numel(children)
    if ~isequal(children(k), ax)
      keptChildren = [keptChildren children(k)];
    end
  end
  set(parent, 'Children', keptChildren);
end
%=============================================================================
function [parent, args] = parseCloudParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1})
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function parent = cloudParent(parent)
  if isempty(parent)
    parent = gcf();
  elseif isgraphics(parent, 'axes')
    error('Nelson:bubblecloud:bubbleCloudCannotBeA', 'BubbleCloud cannot be a child of Axes.');
  elseif isgraphics(parent, 'figure')
    return
  else
    error('Nelson:bubblecloud:invalidParent', 'Invalid parent.');
  end
end
%=============================================================================
function [sizes, labels, groups, groupLevels, source, args] = parseBubbleCloudData(args)
  labels = {};
  groups = {};
  groupLevels = {};
  source = struct('Table', [], 'SizeVariable', [], 'LabelVariable', [], 'GroupVariable', []);
  if isempty(args)
    error('Nelson:bubblecloud:bubbleSizeDataExpected', 'Bubble size data expected.');
  end
  if (istable(args{1}) || istimetable(args{1})) && numel(args) >= 2
    tbl = args{1};
    source.Table = tbl;
    source.SizeVariable = args{2};
    [sizes, sizeName] = graphicsTableVariable(tbl, args{2});
    source.SizeVariable = sizeName;
    args = args(3:end);
    if ~isempty(args) && ~isBubbleCloudPropertyName(args{1})
      source.LabelVariable = args{1};
      [labelValues, labelName] = graphicsTableVariable(tbl, args{1});
      source.LabelVariable = labelName;
      labels = graphicsLabelsFromValue(labelValues, numel(sizes));
      args = args(2:end);
    end
    if ~isempty(args) && ~isBubbleCloudPropertyName(args{1})
      source.GroupVariable = args{1};
      [groupValues, groupName] = graphicsTableVariable(tbl, args{1});
      source.GroupVariable = groupName;
      groups = graphicsLabelsFromValue(groupValues, numel(sizes));
      groupLevels = groupLevelsFromValue(groupValues, groups);
      args = args(2:end);
    end
  elseif istable(args{1}) || istimetable(args{1})
    error('Nelson:bubblecloud:bubbleSizeDataExpected', 'Bubble size data expected.');
  else
    sizes = args{1};
    args = args(2:end);
    if ~isempty(args) && ~isBubbleCloudPropertyName(args{1})
      labels = graphicsLabelsFromValue(args{1}, numel(sizes));
      args = args(2:end);
    end
    if ~isempty(args) && ~isBubbleCloudPropertyName(args{1})
      groups = graphicsLabelsFromValue(args{1}, numel(sizes));
      groupLevels = groupLevelsFromValue(args{1}, groups);
      args = args(2:end);
    end
  end
  if isempty(labels)
    labels = defaultLabels(numel(sizes));
  end
  if isempty(groups)
    groups = repmat({''}, numel(sizes), 1);
  end
  if isempty(groupLevels)
    groupLevels = uniqueStable(groups);
  end
  if ~isnumeric(sizes)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Bubble sizes');
  end
  if ~isvector(sizes)
    error('Nelson:validators:mustBeVector', '%s must be a vector.', 'Bubble sizes');
  end
  sizes = double(sizes(:));
end
%=============================================================================
function levels = groupLevelsFromValue(value, groups)
  if exist('iscategorical', 'file') && iscategorical(value)
    levels = categories(value);
    levels = levels(:);
    present = false(size(levels));
    for k = 1:numel(levels)
      present(k) = any(strcmp(groups, levels{k}));
    end
    levels = levels(present);
  else
    levels = uniqueStable(groups);
  end
end
%=============================================================================
function opts = parseBubbleCloudOptions(args)
  defaultEdgeColor = graphicsDefaultEdgeColor();
  opts = struct('Title', '', 'LegendTitle', '', 'FaceColor', 'flat', 'EdgeColor', defaultEdgeColor);
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'title'
        opts.Title = value;
      case 'legendtitle'
        opts.LegendTitle = value;
      case 'facecolor'
        opts.FaceColor = value;
      case 'edgecolor'
        opts.EdgeColor = validatecolor(value);
      case 'parent'
        error('Nelson:bubblecloud:parentBeforeData', 'Parent must be specified before data.');
      case {'sizedata', 'labeldata', 'groupdata'}
        error('Nelson:bubblecloud:dataPropertiesAreSet', 'Data properties are set from input arguments.');
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'bubblecloud');
    end
  end
end
%=============================================================================
function tf = isBubbleCloudPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  tf = any(strcmpi(char(value), {'Title', 'LegendTitle', 'FaceColor', 'EdgeColor', ...
    'Parent', 'SizeData', 'LabelData', 'GroupData', 'Position', 'InnerPosition', ...
    'OuterPosition', 'Units', 'SourceTable', 'SizeVariable', 'LabelVariable', 'GroupVariable'}));
end
%=============================================================================
function labels = defaultLabels(n)
  labels = cell(n, 1);
  for k = 1:n
    labels{k} = num2str(k);
  end
end
%=============================================================================
function [x, y, radii, bubbleHandles] = drawBubbleCloud(parent, sizes, labels, groups, groupLevels, opts)
  n = numel(sizes);
  radii = bubbleRadii(sizes);
  [x, y] = cloudPositions(radii, groups, groupLevels);
  colors = groupColors(ancestor(parent, 'axes'), groups, groupLevels, opts.FaceColor);
  bubbleHandles = [];
  for k = 1:n
    displayName = legendDisplayName(groups, k);
    [circleX, circleY] = circleData(x(k), y(k), radii(k));
    bubbleHandles = [bubbleHandles, patch('Parent', parent, 'XData', circleX, 'YData', circleY, ...
      'FaceColor', colors(k, :), 'EdgeColor', opts.EdgeColor, 'LineWidth', 1, ...
      'DisplayName', displayName)];
    text(x(k), y(k), labels{k}, 'Parent', parent, 'HorizontalAlignment', 'center', ...
      'VerticalAlignment', 'middle', 'Color', [0 0 0]);
  end
end
%=============================================================================
function [x, y] = circleData(cx, cy, radius)
  theta = (0:96) * 2 * pi / 96;
  x = cx + radius * cos(theta);
  y = cy + radius * sin(theta);
end
%=============================================================================
function radii = bubbleRadii(sizes)
  sizes = abs(sizes(:));
  if all(sizes == 0)
    radii = ones(size(sizes));
  else
    radii = sqrt(sizes) ./ max(sqrt(sizes));
  end
end
%=============================================================================
function [x, y] = cloudPositions(radii, groups, groupLevels)
  if nargin >= 3 && isGroupedBubbleCloud(groupLevels)
    [x, y] = groupedCloudPositions(radii, groups, groupLevels);
    return
  end
  [x, y] = ungroupedCloudPositions(radii);
end
%=============================================================================
function tf = isGroupedBubbleCloud(groupLevels)
  tf = numel(groupLevels) > 1 || (numel(groupLevels) == 1 && ~isempty(groupLevels{1}));
end
%=============================================================================
function [x, y] = ungroupedCloudPositions(radii)
  n = numel(radii);
  x = zeros(n, 1);
  y = zeros(n, 1);
  if n > 3
    [x, y] = packedCloudPositions(radii);
    return
  end
  leftEdge = -radii(1);
  rightEdge = radii(1);
  for k = 2:n
    if rem(k, 2) == 0
      x(k) = leftEdge - radii(k);
      leftEdge = x(k) - radii(k);
    else
      x(k) = rightEdge + radii(k);
      rightEdge = x(k) + radii(k);
    end
  end
end
%=============================================================================
function [x, y] = groupedCloudPositions(radii, groups, groupLevels)
  x = zeros(numel(radii), 1);
  y = zeros(numel(radii), 1);
  groupRadii = zeros(numel(groupLevels), 1);
  for k = 1:numel(groupLevels)
    indices = find(strcmp(groups, groupLevels{k}));
    [localX, localY] = compactCloudPositions(radii(indices));
    x(indices) = localX;
    y(indices) = localY;
    groupRadii(k) = max(hypot(localX, localY) + radii(indices));
  end
  [groupX, groupY] = groupCloudPositions(groupRadii);
  for k = 1:numel(groupLevels)
    indices = find(strcmp(groups, groupLevels{k}));
    x(indices) = x(indices) + groupX(k);
    y(indices) = y(indices) + groupY(k);
  end
  if numel(groupLevels) == 2
    [x, y] = compactTwoBubbleCloudGroups(x, y, radii, groups, groupLevels);
  end
end
%=============================================================================
function [x, y] = compactTwoBubbleCloudGroups(x, y, radii, groups, groupLevels)
  first = find(strcmp(groups, groupLevels{1}));
  second = find(strcmp(groups, groupLevels{2}));
  if isempty(first) || isempty(second)
    return
  end
  shift = -Inf;
  for i = 1:numel(first)
    for j = 1:numel(second)
      leftIndex = first(i);
      rightIndex = second(j);
      shift = max(shift, x(leftIndex) + radii(leftIndex) + radii(rightIndex) - x(rightIndex));
    end
  end
  if isfinite(shift) && shift < 0
    x(second) = x(second) + shift;
  end
end
%=============================================================================
function [x, y] = compactCloudPositions(radii)
  if numel(radii) == 1
    x = 0;
    y = 0;
  else
    [x, y] = packedCloudPositions(radii);
  end
end
%=============================================================================
function [x, y] = groupCloudPositions(radii)
  n = numel(radii);
  x = zeros(n, 1);
  y = zeros(n, 1);
  if n == 1
    return
  elseif n == 2
    x(1) = -radii(1);
    x(2) = radii(2);
  elseif n == 3
    x(2) = radii(2);
    x(3) = -radii(3);
    centers = tangentCircleCenters(x(2), y(2), radii(2) + radii(1), ...
      x(3), y(3), radii(3) + radii(1));
    [x(1), y(1)] = selectPackedCenter(centers, 'upper');
  else
    [x, y] = packedCloudPositions(radii);
  end
  xmin = min(x - radii);
  xmax = max(x + radii);
  ymin = min(y - radii);
  ymax = max(y + radii);
  x = x - (xmin + xmax) / 2;
  y = y - (ymin + ymax) / 2;
end
%=============================================================================
function [x, y] = packedCloudPositions(radii)
  n = numel(radii);
  x = zeros(n, 1);
  y = zeros(n, 1);
  [~, order] = sort(-radii);
  x(order(1)) = -radii(order(1));
  placed = order(1);
  if n >= 2
    second = order(2);
    x(second) = radii(second);
    placed = [placed; second];
  end
  seedPairs = [1 2; 1 2; 2 3; 2 5; 2 6; 2 7; 1 4; 1 3];
  seedSelectors = {'upper', 'lower', 'right', 'right', 'lower', 'lower', 'lower', 'left'};
  seedCount = min(n, size(seedPairs, 1) + 2);
  for k = 3:seedCount
    idx = order(k);
    pair = seedPairs(k - 2, :);
    p = order(pair(1));
    q = order(pair(2));
    [x(idx), y(idx)] = seededPackedPosition(x, y, radii, placed, idx, p, q, ...
      seedSelectors{k - 2});
    placed = [placed; idx];
  end
  for k = seedCount + 1:n
    idx = order(k);
    [x(idx), y(idx)] = bestPackedPosition(x, y, radii, placed, radii(idx));
    placed = [placed; idx];
  end
end
%=============================================================================
function [bestX, bestY] = seededPackedPosition(x, y, radii, placed, idx, p, q, selector)
  centers = tangentCircleCenters(x(p), y(p), radii(p) + radii(idx), ...
    x(q), y(q), radii(q) + radii(idx));
  if isempty(centers)
    [bestX, bestY] = bestPackedPosition(x, y, radii, placed, radii(idx));
    return
  end
  [bestX, bestY] = selectPackedCenter(centers, selector);
  if ~isNonOverlappingPosition(bestX, bestY, radii(idx), x, y, radii, placed)
    [bestX, bestY] = bestPackedPosition(x, y, radii, placed, radii(idx));
  end
end
%=============================================================================
function [x, y] = selectPackedCenter(centers, selector)
  switch selector
    case 'upper'
      [~, idx] = max(centers(:, 2));
    case 'lower'
      [~, idx] = min(centers(:, 2));
    case 'left'
      [~, idx] = min(centers(:, 1));
    otherwise
      [~, idx] = max(centers(:, 1));
  end
  x = centers(idx, 1);
  y = centers(idx, 2);
end
%=============================================================================
function [bestX, bestY] = bestPackedPosition(x, y, radii, placed, radius)
  candidates = packedPositionCandidates(x, y, radii, placed, radius);
  bestScore = Inf;
  bestX = 0;
  bestY = 0;
  for k = 1:size(candidates, 1)
    cx = candidates(k, 1);
    cy = candidates(k, 2);
    if ~isNonOverlappingPosition(cx, cy, radius, x, y, radii, placed)
      continue
    end
    score = packedPositionScore(cx, cy, radius, x, y, radii, placed);
    if score < bestScore
      bestScore = score;
      bestX = cx;
      bestY = cy;
    end
  end
end
%=============================================================================
function candidates = packedPositionCandidates(x, y, radii, placed, radius)
  candidates = [];
  for i = 1:numel(placed)
    idx = placed(i);
    distance = radii(idx) + radius;
    candidates = [candidates; x(idx) + distance, y(idx); x(idx) - distance, y(idx); ...
      x(idx), y(idx) + distance; x(idx), y(idx) - distance];
  end
  for i = 1:numel(placed)
    for j = i + 1:numel(placed)
      p = placed(i);
      q = placed(j);
      candidates = [candidates; tangentCircleCenters(x(p), y(p), radii(p) + radius, ...
        x(q), y(q), radii(q) + radius)];
    end
  end
end
%=============================================================================
function centers = tangentCircleCenters(x1, y1, r1, x2, y2, r2)
  centers = [];
  dx = x2 - x1;
  dy = y2 - y1;
  d = hypot(dx, dy);
  if d <= eps
    return
  end
  a = (r1 ^ 2 - r2 ^ 2 + d ^ 2) / (2 * d);
  h2 = r1 ^ 2 - a ^ 2;
  if h2 < -1e-12
    return
  end
  h = sqrt(max(0, h2));
  xm = x1 + a * dx / d;
  ym = y1 + a * dy / d;
  ox = -dy * h / d;
  oy = dx * h / d;
  centers = [xm + ox, ym + oy; xm - ox, ym - oy];
end
%=============================================================================
function tf = isNonOverlappingPosition(cx, cy, radius, x, y, radii, placed)
  tf = true;
  for k = 1:numel(placed)
    idx = placed(k);
    if hypot(cx - x(idx), cy - y(idx)) < radius + radii(idx) - 1e-10
      tf = false;
      return
    end
  end
end
%=============================================================================
function score = packedPositionScore(cx, cy, radius, x, y, radii, placed)
  px = [x(placed); cx];
  py = [y(placed); cy];
  pr = [radii(placed); radius];
  width = max(px + pr) - min(px - pr);
  height = max(py + pr) - min(py - pr);
  score = width * height + 0.12 * (width - height) ^ 2 + 0.02 * hypot(cx, cy);
end
%=============================================================================
function updateBubbleCloudAxes(ax, x, y, radii)
  xMargin = 0.11;
  yMargin = 0.45;
  ax.XLim = [min(x - radii) - xMargin, max(x + radii) + xMargin];
  ax.YLim = [min(y - radii) - yMargin, max(y + radii) + yMargin];
  ax.XTick = [];
  ax.YTick = [];
  ax.Box = 'on';
  ax.Position = [0.13 0.11 0.775 0.815];
  ax.XColor = 'none';
  ax.YColor = 'none';
end
%=============================================================================
function updateBubbleCloudLegend(ax, bubbleHandles, groups, groupLevels, opts)
  levels = groupLevels;
  if numel(levels) <= 1 || isempty(levels{1})
    return
  end
  firstHandles = [];
  for k = 1:numel(levels)
    idx = find(strcmp(groups, levels{k}), 1);
    firstHandles = [firstHandles, line('Parent', ax, 'XData', NaN, 'YData', NaN, ...
      'LineStyle', 'none', 'Marker', 'o', ...
      'MarkerSize', 6, 'MarkerFaceColor', bubbleHandles(idx).FaceColor, ...
      'MarkerEdgeColor', bubbleHandles(idx).EdgeColor)];
  end
  lgd = legend(ax, firstHandles, levels);
  if ~isempty(opts.LegendTitle)
    title(lgd, opts.LegendTitle);
  end
  positionBubbleCloudLegend(ax, lgd);
  installBubbleCloudResizeFcn(ax, lgd);
end
%=============================================================================
function positionBubbleCloudLegend(ax, lgd)
  if isempty(lgd) || ~isgraphics(lgd)
    return
  end
  fig = ancestor(ax, 'figure');
  if isempty(fig) || ~isgraphics(fig)
    return
  end
  figPosition = fig.Position;
  plotBox = bubbleCloudPlotBoxPixels(ax, figPosition);
  legendUnits = lgd.Units;
  legendPosition = lgd.Position;
  if strcmpi(legendUnits, 'pixels')
    legendWidth = legendPosition(3);
    legendHeight = legendPosition(4);
  else
    legendWidth = legendPosition(3) * figPosition(3);
    legendHeight = legendPosition(4) * figPosition(4);
  end
  [legendWidth, legendHeight] = bubbleCloudLegendSize(lgd, legendWidth, legendHeight);
  margin = 8;
  x = figPosition(3) - legendWidth - margin;
  y = figPosition(4) - legendHeight - margin;
  lgd.Units = 'pixels';
  lgd.Position = [x, y, legendWidth, legendHeight];
end
%=============================================================================
function [legendWidth, legendHeight] = bubbleCloudLegendSize(lgd, legendWidth, legendHeight)
  labels = lgd.String;
  if ischar(labels)
    labels = {labels};
  end
  labelCount = numel(labels);
  maxChars = 0;
  for k = 1:labelCount
    maxChars = max(maxChars, length(char(labels{k})));
  end
  titleString = '';
  if isgraphics(lgd.Title)
    titleString = lgd.Title.String;
  end
  hasTitle = ~isempty(titleString);
  maxChars = max(maxChars, length(char(titleString)));
  iconWidth = 30;
  try
    iconWidth = lgd.IconColumnWidth;
  catch
  end
  compactWidth = max(36, iconWidth + 22 + 6 * maxChars);
  compactHeight = 8 + 13 * max(1, labelCount);
  if hasTitle
    compactHeight = compactHeight + 15;
  end
  legendWidth = min(legendWidth, compactWidth);
  legendHeight = min(legendHeight, compactHeight);
end
%=============================================================================
function plotBox = bubbleCloudPlotBoxPixels(ax, figPosition)
  axesPosition = ax.Position;
  plotBox = [axesPosition(1) * figPosition(3), axesPosition(2) * figPosition(4), ...
    axesPosition(3) * figPosition(3), axesPosition(4) * figPosition(4)];
  xlim = ax.XLim;
  ylim = ax.YLim;
  dataWidth = xlim(2) - xlim(1);
  dataHeight = ylim(2) - ylim(1);
  if dataWidth <= 0 || dataHeight <= 0 || plotBox(3) <= 0 || plotBox(4) <= 0
    return
  end
  dataAspect = dataWidth / dataHeight;
  boxAspect = plotBox(3) / plotBox(4);
  if boxAspect > dataAspect
    width = plotBox(4) * dataAspect;
    plotBox(1) = plotBox(1) + (plotBox(3) - width) / 2;
    plotBox(3) = width;
  else
    height = plotBox(3) / dataAspect;
    plotBox(2) = plotBox(2) + (plotBox(4) - height) / 2;
    plotBox(4) = height;
  end
end
%=============================================================================
function installBubbleCloudResizeFcn(ax, lgd)
  fig = ancestor(ax, 'figure');
  if isempty(fig) || ~isgraphics(fig) || ~isempty(fig.SizeChangedFcn)
    return
  end
  fig.SizeChangedFcn = @(src, event) positionBubbleCloudLegend(ax, lgd);
end
%=============================================================================
function displayName = legendDisplayName(groups, k)
  if k == find(strcmp(groups, groups{k}), 1)
    displayName = groups{k};
  else
    displayName = '';
  end
end
%=============================================================================
function colors = groupColors(ax, groups, groupLevels, faceColor)
  n = numel(groups);
  if ~ischar(faceColor) || ~strcmpi(faceColor, 'flat')
    colors = repmat(validatecolor(faceColor), n, 1);
    return
  end
  levels = groupLevels;
  order = ax.ColorOrder;
  colors = zeros(n, 3);
  for k = 1:n
    idx = find(strcmp(groups{k}, levels), 1);
    colors(k, :) = 0.65 * order(1 + mod(idx - 1, size(order, 1)), :) + 0.35;
  end
end
%=============================================================================
function levels = uniqueStable(values)
  levels = {};
  for k = 1:numel(values)
    if ~any(strcmp(values{k}, levels))
      levels{end + 1, 1} = values{k};
    end
  end
end
%=============================================================================
