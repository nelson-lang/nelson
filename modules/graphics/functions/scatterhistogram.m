%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = scatterhistogram(varargin)
  narginchk(2, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  [data, args] = parseScatterHistogramData(args);
  opts = parseScatterHistogramOptions(args, data);
  parent = scatterHistogramParent(parent);
  chart = createScatterHistogramChart(parent, data, opts);
  [axScatter, axTop, axRight] = createScatterHistogramAxes(parent, opts);
  set(chart, 'Children', [axScatter, axTop, axRight]);
  state = renderScatterHistogram(chart, data, opts, axScatter, axTop, axRight);
  chart.UserData = state;
  axes(axScatter);
  removeScatterHistogramAxesFromFigure(parent, [axScatter, axTop, axRight]);
  if nargout == 1
    varargout{1} = chart;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    error('Nelson:scatterhistogram:scatterHistogramChartCannotBeA', 'ScatterHistogramChart cannot be a child of Axes.');
  end
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'figure')
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function parent = scatterHistogramParent(parent)
  if isempty(parent)
    parent = figure();
  end
end
%=============================================================================
function [data, args] = parseScatterHistogramData(args)
  if isempty(args)
    error('Nelson:graphics:dataExpected', 'Data expected.');
  end
  data = defaultScatterHistogramData();
  if (istable(args{1}) || istimetable(args{1}))
    [data, args] = parseTableScatterHistogramData(args, data);
  else
    [data, args] = parseVectorScatterHistogramData(args, data);
  end
  if numel(data.XOriginal) ~= numel(data.YOriginal)
    error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'X', 'Y');
  end
  data.XOriginal = data.XOriginal(:);
  data.YOriginal = data.YOriginal(:);
  [data.XRender, data.XCategories] = renderValues(data.XOriginal, 'X');
  [data.YRender, data.YCategories] = renderValues(data.YOriginal, 'Y');
end
%=============================================================================
function data = defaultScatterHistogramData()
  data = struct();
  data.SourceTable = [];
  data.XVariable = [];
  data.YVariable = [];
  data.GroupVariable = [];
  data.GroupData = [];
  data.XOriginal = [];
  data.YOriginal = [];
  data.XRender = [];
  data.YRender = [];
  data.XCategories = {};
  data.YCategories = {};
end
%=============================================================================
function [data, args] = parseTableScatterHistogramData(args, data)
  if numel(args) < 3
    error('Nelson:scatterhistogram:tableAndVariableExpected', 'Table and variable arguments expected.');
  end
  tbl = args{1};
  [x, xName] = graphicsTableVariable(tbl, args{2});
  [y, yName] = graphicsTableVariable(tbl, args{3});
  data.SourceTable = tbl;
  data.XVariable = xName;
  data.YVariable = yName;
  data.XOriginal = x;
  data.YOriginal = y;
  args = args(4:end);
end
%=============================================================================
function [data, args] = parseVectorScatterHistogramData(args, data)
  if numel(args) < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  data.XOriginal = args{1};
  data.YOriginal = args{2};
  args = args(3:end);
end
%=============================================================================
function [values, categoryLabels] = renderValues(values, axisName)
  categoryLabels = {};
  if iscategorical(values)
    categoryLabels = cellstr(categories(values));
    values = double(values);
  elseif isnumeric(values) || islogical(values)
    values = double(values);
  else
    error('Nelson:scatterhistogram:invalidType', '%s data must be numeric, logical, or categorical.', axisName);
  end
  values = values(:);
end
%=============================================================================
function opts = parseScatterHistogramOptions(args, data)
  opts = defaultScatterHistogramOptions();
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'numbins'
        opts.NumBins = validateNumBins(value);
      case 'binwidths'
        opts.BinWidths = validateBinWidths(value);
      case 'markerstyle'
        opts.MarkerStyle = cellstrValue(value);
      case 'markersize'
        opts.MarkerSize = validateNonnegativeVector(value, 'MarkerSize');
      case 'markeralpha'
        opts.MarkerAlpha = validateAlphaVector(value, 'MarkerAlpha');
      case {'markercolor', 'color'}
        opts.Color = normalizedColorList(value);
      case 'histogramdisplaystyle'
        opts.HistogramDisplayStyle = validateHistogramDisplayStyle(value);
      case 'markerfilled'
        opts.MarkerFilled = validateOnOff(value, 'MarkerFilled');
      case 'groupdata'
        opts.GroupData = value(:);
      case 'groupvariable'
        if isempty(data.SourceTable)
          error('Nelson:scatterhistogram:groupVariableIsSupportedOnly', 'GroupVariable is supported only for table input.');
        end
        [groupData, groupName] = graphicsTableVariable(data.SourceTable, value);
        opts.GroupData = groupData(:);
        opts.GroupVariable = groupName;
      case 'legendvisible'
        opts.LegendVisible = validateOnOff(value, 'LegendVisible');
      case 'legendtitle'
        opts.LegendTitle = char(value);
      case 'title'
        opts.Title = char(value);
      case 'xlabel'
        opts.XLabel = char(value);
      case 'ylabel'
        opts.YLabel = char(value);
      case 'linewidth'
        opts.LineWidth = validatePositiveVector(value, 'LineWidth');
      case 'linestyle'
        opts.LineStyle = cellstrValue(value);
      case 'scatterplotlocation'
        opts.ScatterPlotLocation = validateScatterPlotLocation(value);
      case 'scatterplotproportion'
        opts.ScatterPlotProportion = validateProportion(value);
      case 'xhistogramdirection'
        opts.XHistogramDirection = validateDirection(value, {'up', 'down'}, 'XHistogramDirection');
      case 'yhistogramdirection'
        opts.YHistogramDirection = validateDirection(value, {'left', 'right'}, 'YHistogramDirection');
      case 'xlimits'
        opts.XLimits = validateLimits(value, 'XLimits');
      case 'ylimits'
        opts.YLimits = validateLimits(value, 'YLimits');
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'scatterhistogram');
    end
  end
  if ~isempty(opts.GroupData) && numel(opts.GroupData) ~= numel(data.XOriginal)
    error('Nelson:scatterhistogram:groupDataOneValuePerObs', 'GroupData must have one value per observation.');
  end
  if isempty(opts.GroupVariable)
    opts.GroupVariable = data.GroupVariable;
  end
end
%=============================================================================
function opts = defaultScatterHistogramOptions()
  opts = struct('NumBins', [10; 10], 'BinWidths', [], 'MarkerStyle', {{'o'}}, ...
    'MarkerSize', 36, 'MarkerAlpha', 1, 'Color', [0.066 0.443 0.745], ...
    'HistogramDisplayStyle', 'stairs', 'MarkerFilled', 'on', 'GroupData', [], ...
    'GroupVariable', [], 'LegendVisible', 'off', 'LegendTitle', '', 'Title', '', ...
    'XLabel', '', 'YLabel', '', 'LineWidth', 0.5, 'LineStyle', {{'-'}}, ...
    'ScatterPlotLocation', 'SouthWest', 'ScatterPlotProportion', 0.75, ...
    'XHistogramDirection', 'up', 'YHistogramDirection', 'right', ...
    'XLimits', [], 'YLimits', []);
end
%=============================================================================
function chart = createScatterHistogramChart(parent, data, opts)
  color = firstColor(opts.Color);
  chartArgs = {'AutoParent', 'off', 'Parent', parent, ...
    'XData', data.XOriginal, 'YData', data.YOriginal, 'Color', color, ...
    'NumBins', opts.NumBins, 'HistogramDisplayStyle', opts.HistogramDisplayStyle, ...
    'MarkerStyle', firstCellValue(opts.MarkerStyle), ...
    'MarkerSize', firstNumericValue(opts.MarkerSize), ...
    'MarkerFilled', opts.MarkerFilled, ...
    'MarkerAlpha', firstNumericValue(opts.MarkerAlpha), ...
    'LineStyle', firstCellValue(opts.LineStyle), ...
    'LineWidth', firstNumericValue(opts.LineWidth), ...
    'ScatterPlotLocation', opts.ScatterPlotLocation, ...
    'ScatterPlotProportion', opts.ScatterPlotProportion, ...
    'XHistogramDirection', opts.XHistogramDirection, ...
    'YHistogramDirection', opts.YHistogramDirection, ...
    'LegendVisible', opts.LegendVisible, ...
    'LegendTitle', opts.LegendTitle, 'Title', opts.Title, ...
    'XLabel', opts.XLabel, 'YLabel', opts.YLabel};
  if ~isempty(data.SourceTable)
    chartArgs = [chartArgs, {'SourceTable', data.SourceTable}];
  end
  if ~isempty(data.XVariable)
    chartArgs = [chartArgs, {'XVariable', data.XVariable}];
  end
  if ~isempty(data.YVariable)
    chartArgs = [chartArgs, {'YVariable', data.YVariable}];
  end
  if ~isempty(opts.GroupData)
    chartArgs = [chartArgs, {'GroupData', opts.GroupData}];
  end
  if ~isempty(opts.GroupVariable)
    chartArgs = [chartArgs, {'GroupVariable', opts.GroupVariable}];
  end
  if ~isempty(opts.XLimits)
    chartArgs = [chartArgs, {'XLimits', opts.XLimits}];
  end
  if ~isempty(opts.YLimits)
    chartArgs = [chartArgs, {'YLimits', opts.YLimits}];
  end
  chart = __scatterhistogram__(chartArgs{:});
end
%=============================================================================
function [axScatter, axTop, axRight] = createScatterHistogramAxes(parent, opts)
  positions = scatterHistogramAxesPositions(opts);
  axScatter = axes(parent, 'Position', positions.Scatter);
  axTop = axes(parent, 'Position', positions.XHistogram);
  axRight = axes(parent, 'Position', positions.YHistogram);
end
%=============================================================================
function removeScatterHistogramAxesFromFigure(parent, axesHandles)
  if ~isgraphics(parent, 'figure')
    return
  end
  children = get(parent, 'Children');
  keptChildren = [];
  for k = 1:numel(children)
    keepChild = true;
    for j = 1:numel(axesHandles)
      if isequal(children(k), axesHandles(j))
        keepChild = false;
        break
      end
    end
    if keepChild
      keptChildren = [keptChildren children(k)];
    end
  end
  set(parent, 'Children', keptChildren);
end
%=============================================================================
function positions = scatterHistogramAxesPositions(opts)
  left = 0.10;
  bottom = 0.10;
  width = 0.84;
  height = 0.80;
  gap = 0.012;
  scatterRatio = opts.ScatterPlotProportion;
  histRatio = 1 - scatterRatio;
  scatterW = width * scatterRatio;
  scatterH = height * scatterRatio;
  histW = max(width * histRatio - gap, 0.05);
  histH = max(height * histRatio - gap, 0.05);
  switch lower(opts.ScatterPlotLocation)
    case 'southwest'
      scatter = [left bottom scatterW scatterH];
      xhist = [left bottom + scatterH + gap scatterW histH];
      yhist = [left + scatterW + gap bottom histW scatterH];
    case 'southeast'
      scatter = [left + histW + gap bottom scatterW scatterH];
      xhist = [left + histW + gap bottom + scatterH + gap scatterW histH];
      yhist = [left bottom histW scatterH];
    case 'northeast'
      scatter = [left + histW + gap bottom + histH + gap scatterW scatterH];
      xhist = [left + histW + gap bottom scatterW histH];
      yhist = [left bottom + histH + gap histW scatterH];
    otherwise
      scatter = [left bottom + histH + gap scatterW scatterH];
      xhist = [left bottom scatterW histH];
      yhist = [left + scatterW + gap bottom + histH + gap histW scatterH];
  end
  positions = struct('Scatter', scatter, 'XHistogram', xhist, 'YHistogram', yhist);
end
%=============================================================================
function state = renderScatterHistogram(chart, data, opts, axScatter, axTop, axRight)
  groups = scatterHistogramGroups(opts.GroupData, numel(data.XRender));
  colors = colorCycle(opts.Color, groups.Count);
  scatterHandles = [];
  xHistogramHandles = [];
  yHistogramHandles = [];
  % Every group adds one more series to each of the three axes, so they are held
  % for the whole loop: left on 'replace' they would drop the previous group.
  savedScatterNextPlot = axScatter.NextPlot;
  savedTopNextPlot = axTop.NextPlot;
  savedRightNextPlot = axRight.NextPlot;
  axScatter.NextPlot = 'add';
  axTop.NextPlot = 'add';
  axRight.NextPlot = 'add';
  for k = 1:groups.Count
    indices = groups.Index == k;
    color = colors(k, :);
    scatterHandles = [scatterHandles, renderScatterGroup(axScatter, data, opts, indices, color, k, groups)];
    xHistogramHandles = [xHistogramHandles, renderXHistogram(axTop, data.XRender(indices), opts, color, k, groups)];
    yHistogramHandles = [yHistogramHandles, renderYHistogram(axRight, data.YRender(indices), opts, color, k, groups)];
  end
  axScatter.NextPlot = savedScatterNextPlot;
  axTop.NextPlot = savedTopNextPlot;
  axRight.NextPlot = savedRightNextPlot;
  applyScatterHistogramAxesState(axScatter, axTop, axRight, data, opts, xHistogramHandles, yHistogramHandles);
  updateChartComputedProperties(chart, axScatter, xHistogramHandles, yHistogramHandles, opts);
  applyLabels(axScatter, chart, opts, data);
  applyLegend(axScatter, opts, groups);
  state = scatterHistogramState(chart, data, opts, groups, scatterHandles, ...
    xHistogramHandles, yHistogramHandles, axScatter, axTop, axRight);
end
%=============================================================================
function h = renderScatterGroup(ax, data, opts, indices, color, groupIndex, groups)
  axes(ax);
  marker = cycledCellValue(opts.MarkerStyle, groupIndex);
  markerSize = cycledNumericValue(opts.MarkerSize, groupIndex);
  markerAlpha = cycledNumericValue(opts.MarkerAlpha, groupIndex);
  scatterArgs = {'Marker', marker, 'SizeData', markerSize, ...
    'MarkerEdgeAlpha', markerAlpha, 'MarkerFaceAlpha', markerAlpha, ...
    'DisplayName', groups.Labels{groupIndex}};
  if strcmp(opts.MarkerFilled, 'on')
    scatterArgs = [scatterArgs, {'filled'}];
  end
  scatterArgs = [scatterArgs, {'MarkerEdgeColor', color, 'CData', color}];
  h = scatter(ax, data.XRender(indices), data.YRender(indices), scatterArgs{:});
end
%=============================================================================
function h = renderXHistogram(ax, values, opts, color, groupIndex, groups)
  axes(ax);
  if strcmp(opts.HistogramDisplayStyle, 'smooth')
    h = renderSmoothHistogram(ax, values, opts, color, groupIndex, false, groups);
  else
    binArgs = histogramBinArguments(opts, groupIndex, false);
    h = histogram(ax, finiteValues(values), binArgs{:}, ...
      'DisplayStyle', opts.HistogramDisplayStyle, 'EdgeColor', markerMatchedColor(color), ...
      'LineWidth', cycledNumericValue(opts.LineWidth, groupIndex), ...
      'LineStyle', cycledCellValue(opts.LineStyle, groupIndex), ...
      'DisplayName', groups.Labels{groupIndex});
  end
end
%=============================================================================
function h = renderYHistogram(ax, values, opts, color, groupIndex, groups)
  axes(ax);
  if strcmp(opts.HistogramDisplayStyle, 'smooth')
    h = renderSmoothHistogram(ax, values, opts, color, groupIndex, true, groups);
  else
    binArgs = histogramBinArguments(opts, groupIndex, true);
    h = histogram(ax, finiteValues(values), binArgs{:}, ...
      'DisplayStyle', opts.HistogramDisplayStyle, 'Orientation', 'horizontal', ...
      'EdgeColor', markerMatchedColor(color), 'LineWidth', cycledNumericValue(opts.LineWidth, groupIndex), ...
      'LineStyle', cycledCellValue(opts.LineStyle, groupIndex), ...
      'DisplayName', groups.Labels{groupIndex});
  end
end
%=============================================================================
function h = renderSmoothHistogram(ax, values, opts, color, groupIndex, horizontal, groups)
  [centers, widths] = distributionDensityShape(values, 1, cycledNumBins(opts.NumBins, groupIndex, horizontal));
  if horizontal
    h = line(ax, widths, centers, 'Color', color, ...
      'LineWidth', cycledNumericValue(opts.LineWidth, groupIndex), ...
      'LineStyle', cycledCellValue(opts.LineStyle, groupIndex), ...
      'DisplayName', groups.Labels{groupIndex});
  else
    h = line(ax, centers, widths, 'Color', color, ...
      'LineWidth', cycledNumericValue(opts.LineWidth, groupIndex), ...
      'LineStyle', cycledCellValue(opts.LineStyle, groupIndex), ...
      'DisplayName', groups.Labels{groupIndex});
  end
end
%=============================================================================
function args = histogramBinArguments(opts, groupIndex, horizontal)
  if isempty(opts.BinWidths)
    args = {cycledNumBins(opts.NumBins, groupIndex, horizontal)};
  else
    row = 1 + horizontal;
    widths = opts.BinWidths;
    if isscalar(widths)
      width = widths;
    elseif isvector(widths)
      width = widths(min(row, numel(widths)));
    else
      width = widths(row, 1 + mod(groupIndex - 1, size(widths, 2)));
    end
    args = {'BinWidth', width};
  end
end
%=============================================================================
function applyScatterHistogramAxesState(axScatter, axTop, axRight, data, opts, xHist, yHist)
  xLimits = opts.XLimits;
  yLimits = opts.YLimits;
  if isempty(xLimits)
    xLimits = paddedLimits(data.XRender);
  end
  if isempty(yLimits)
    yLimits = paddedLimits(data.YRender);
  end
  if ~isempty(xLimits)
    axScatter.XLim = xLimits;
    axTop.XLim = xLimits;
  end
  if ~isempty(yLimits)
    axScatter.YLim = yLimits;
    axRight.YLim = yLimits;
  end
  applyNumericTicks(axScatter, axTop, axRight, data);
  applyCategoricalTicks(axScatter, axTop, axRight, data);
  hideMarginalAxisLabels(axTop, axRight);
  applyHistogramCountLimits(axTop, axRight, xHist, yHist, opts);
end
%=============================================================================
function updateChartComputedProperties(chart, axScatter, xHist, yHist, opts)
  chart.XLimits = axScatter.XLim;
  chart.YLimits = axScatter.YLim;
  if isempty(opts.BinWidths) && ~strcmp(opts.HistogramDisplayStyle, 'smooth') ...
      && ~isempty(xHist) && ~isempty(yHist)
    chart.BinWidths = [xHist(1).BinWidth yHist(1).BinWidth];
  elseif ~isempty(opts.BinWidths)
    chart.BinWidths = opts.BinWidths(:).';
  end
end
%=============================================================================
function applyCategoricalTicks(axScatter, axTop, axRight, data)
  if ~isempty(data.XCategories)
    ticks = 1:numel(data.XCategories);
    axScatter.XTick = ticks;
    axScatter.XTickLabel = data.XCategories;
    axTop.XTick = ticks;
  end
  if ~isempty(data.YCategories)
    ticks = 1:numel(data.YCategories);
    axScatter.YTick = ticks;
    axScatter.YTickLabel = data.YCategories;
    axRight.YTick = ticks;
  end
end
%=============================================================================
function applyNumericTicks(axScatter, axTop, axRight, data)
  if isempty(data.XCategories)
    ticks = niceLinearTicks(axScatter.XLim);
    axScatter.XTick = ticks;
    axTop.XTick = ticks;
  end
  if isempty(data.YCategories)
    ticks = niceLinearTicks(axScatter.YLim);
    axScatter.YTick = ticks;
    axRight.YTick = ticks;
  end
end
%=============================================================================
function ticks = niceLinearTicks(limits)
  if numel(limits) ~= 2 || any(~isfinite(limits)) || limits(1) >= limits(2)
    ticks = [];
    return
  end
  span = limits(2) - limits(1);
  rawStep = span / 6;
  magnitude = 10 ^ floor(log10(rawStep));
  residual = rawStep / magnitude;
  if residual <= 1.5
    step = magnitude;
  elseif residual <= 3
    step = 2 * magnitude;
  elseif residual <= 7
    step = 5 * magnitude;
  else
    step = 10 * magnitude;
  end
  ticks = ceil(limits(1) / step) * step:step:floor(limits(2) / step) * step;
  if numel(ticks) < 2
    ticks = linspace(limits(1), limits(2), 5);
  elseif numel(ticks) > 8
    step = step * ceil(numel(ticks) / 8);
    ticks = ceil(limits(1) / step) * step:step:floor(limits(2) / step) * step;
  end
  ticks(abs(ticks) < step * 1e-10) = 0;
end
%=============================================================================
function hideMarginalAxisLabels(axTop, axRight)
  axTop.XTickLabel = {};
  axTop.YTick = [];
  axTop.YTickLabel = {};
  axRight.XTick = [];
  axRight.XTickLabel = {};
  axRight.YTickLabel = {};
end
%=============================================================================
function applyHistogramCountLimits(axTop, axRight, xHist, yHist, opts)
  if strcmp(opts.HistogramDisplayStyle, 'smooth')
    axTop.YLim = [0 1];
    axRight.XLim = [0 1];
    return
  end
  xMax = maxHistogramCounts(xHist);
  yMax = maxHistogramCounts(yHist);
  if xMax > 0
    axTop.YLim = [0 xMax];
  end
  if yMax > 0
    axRight.XLim = [0 yMax];
  end
end
%=============================================================================
function applyLabels(ax, chart, opts, data)
  if ~isempty(opts.Title)
    title(ax, opts.Title);
  end
  xText = opts.XLabel;
  yText = opts.YLabel;
  if isempty(xText) && ~isempty(data.XVariable)
    xText = data.XVariable;
  end
  if isempty(yText) && ~isempty(data.YVariable)
    yText = data.YVariable;
  end
  if ~isempty(xText)
    xlabel(ax, xText);
    chart.XLabel = xText;
  end
  if ~isempty(yText)
    ylabel(ax, yText);
    chart.YLabel = yText;
  end
end
%=============================================================================
function applyLegend(ax, opts, groups)
  if groups.Count < 2 || ~strcmp(opts.LegendVisible, 'on')
    return
  end
  lgd = legend(ax, groups.Labels);
  if ~isempty(opts.LegendTitle) && isprop(lgd, 'Title')
    lgd.Title = opts.LegendTitle;
  end
end
%=============================================================================
function state = scatterHistogramState(chart, data, opts, groups, scatterHandles, xHist, yHist, axScatter, axTop, axRight)
  state = struct();
  state.ChartType = 'scatterhistogram';
  state.Chart = chart;
  state.Scatter = scatterHandles;
  state.XHistogram = xHist;
  state.YHistogram = yHist;
  state.ScatterAxes = axScatter;
  state.XHistogramAxes = axTop;
  state.YHistogramAxes = axRight;
  state.XData = data.XOriginal;
  state.YData = data.YOriginal;
  state.XRenderData = data.XRender;
  state.YRenderData = data.YRender;
  state.XCategories = data.XCategories;
  state.YCategories = data.YCategories;
  state.Groups = groups;
  state.Options = opts;
end
%=============================================================================
function groups = scatterHistogramGroups(groupData, n)
  if isempty(groupData)
    groups.Index = ones(n, 1);
    groups.Values = [];
    groups.Labels = {''};
    groups.Count = 1;
    return
  end
  groupData = groupData(:);
  labels = groupLabels(groupData);
  values = {};
  index = zeros(numel(groupData), 1);
  for k = 1:numel(groupData)
    label = labels{k};
    pos = find(strcmp(values, label), 1);
    if isempty(pos)
      values{end + 1} = label;
      pos = numel(values);
    end
    index(k) = pos;
  end
  groups.Index = index;
  groups.Values = values;
  groups.Labels = values;
  groups.Count = numel(values);
end
%=============================================================================
function labels = groupLabels(groupData)
  if iscategorical(groupData)
    labels = cellstr(string(groupData));
  elseif isstring(groupData)
    labels = cellstr(groupData);
  elseif iscellstr(groupData)
    labels = groupData(:);
  elseif islogical(groupData)
    labels = cellstr(string(groupData));
  elseif isnumeric(groupData)
    labels = cellstr(string(groupData));
  else
    error('Nelson:scatterhistogram:invalidGroupData', 'GroupData must be numeric, logical, categorical, string, or cellstr.');
  end
end
%=============================================================================
function values = finiteValues(values)
  values = values(isfinite(values));
end
%=============================================================================
function limits = paddedLimits(values)
  values = finiteValues(values);
  if isempty(values)
    limits = [0 1];
    return
  end
  low = min(values);
  high = max(values);
  if low == high
    limits = [low - 0.5 high + 0.5];
  else
    pad = (high - low) * 0.05;
    limits = [low - pad high + pad];
  end
end
%=============================================================================
function count = maxHistogramCounts(handles)
  count = 0;
  for k = 1:numel(handles)
    if isprop(handles(k), 'BinCounts')
      count = max(count, max(handles(k).BinCounts));
    end
  end
end
%=============================================================================
function value = validateNumBins(value)
  if ~isnumeric(value) || isempty(value) || any(value(:) < 1) || any(~isfinite(value(:)))
    error('Nelson:scatterhistogram:numBinsScalarVectorMatrix', 'NumBins must be a positive scalar, vector, or matrix.');
  end
  value = floor(double(value));
  if isscalar(value)
    value = [value; value];
  elseif isvector(value)
    value = value(:);
    if numel(value) ~= 2
      error('Nelson:graphics:invalidNumBins', 'NumBins must be a positive scalar or two-element vector.');
    end
  elseif size(value, 1) ~= 2
    error('Nelson:scatterhistogram:invalidNumBinsMatrix', 'NumBins matrix must have two rows.');
  end
end
%=============================================================================
function value = validateBinWidths(value)
  if ~isnumeric(value) || isempty(value) || any(value(:) <= 0) || any(~isfinite(value(:)))
    error('Nelson:scatterhistogram:invalidBinWidths', 'BinWidths must contain positive finite values.');
  end
  value = double(value);
end
%=============================================================================
function value = validatePositiveVector(value, name)
  if ~isnumeric(value) || isempty(value) || any(value(:) <= 0) || any(~isfinite(value(:)))
    error('Nelson:scatterhistogram:mustBePositiveFinite', '%s must contain positive finite values.', name);
  end
  value = double(value(:).');
end
%=============================================================================
function value = validateNonnegativeVector(value, name)
  if ~isnumeric(value) || isempty(value) || any(value(:) < 0) || any(~isfinite(value(:)))
    error('Nelson:scatterhistogram:mustBeNonnegativeFinite', '%s must contain nonnegative finite values.', name);
  end
  value = double(value(:).');
end
%=============================================================================
function value = validateAlphaVector(value, name)
  value = validateNonnegativeVector(value, name);
  if any(value > 1)
    error('Nelson:scatterhistogram:mustBeInRange', '%s must contain values between 0 and 1.', name);
  end
end
%=============================================================================
function value = validateHistogramDisplayStyle(value)
  value = lower(char(value));
  if ~(strcmp(value, 'bar') || strcmp(value, 'stairs') || strcmp(value, 'smooth'))
    error('Nelson:scatterhistogram:invalidHistogramDisplayStyle', 'HistogramDisplayStyle must be ''bar'', ''stairs'', or ''smooth''.');
  end
end
%=============================================================================
function value = validateOnOff(value, name)
  if islogical(value) && isscalar(value)
    value = ternary(value, 'on', 'off');
    return
  end
  if ischar(value) || isStringScalar(value)
    value = lower(char(value));
    if strcmp(value, 'on') || strcmp(value, 'off')
      return
    end
  end
  error('Nelson:scatterhistogram:mustBeOnOff', '%s must be ''on'' or ''off''.', name);
end
%=============================================================================
function value = validateScatterPlotLocation(value)
  allowed = {'southwest', 'southeast', 'northeast', 'northwest'};
  raw = char(value);
  if ~any(strcmp(lower(raw), allowed))
    error('Nelson:scatterhistogram:invalidScatterPlotLocation', 'ScatterPlotLocation must be ''SouthWest'', ''SouthEast'', ''NorthEast'', or ''NorthWest''.');
  end
  value = canonicalChoice(raw, allowed);
end
%=============================================================================
function value = validateDirection(value, allowed, name)
  raw = char(value);
  if ~any(strcmp(lower(raw), allowed))
    error('Nelson:scatterhistogram:invalidValue', '%s has an unsupported value.', name);
  end
  value = lower(raw);
end
%=============================================================================
function value = validateProportion(value)
  if ~isnumeric(value) || ~isscalar(value) || value <= 0 || value >= 1 || ~isfinite(value)
    error('Nelson:scatterhistogram:invalidScatterPlotProportion', '%s must be a scalar between 0 and 1.', 'ScatterPlotProportion');
  end
  value = double(value);
end
%=============================================================================
function value = validateLimits(value, name)
  if ~isnumeric(value) || numel(value) ~= 2 || any(~isfinite(value(:))) || value(1) >= value(2)
    error('Nelson:graphics:mustBeTwoElementVector', '%s must be a two-element increasing finite vector.', name);
  end
  value = double(value(:).');
end
%=============================================================================
function values = cellstrValue(value)
  if ischar(value) || isStringScalar(value)
    values = {char(value)};
  elseif isstring(value) || iscellstr(value)
    values = cellstr(value(:).');
  else
    error('Nelson:graphics:textValueExpected', 'Text value expected.');
  end
end
%=============================================================================
function colors = normalizedColorList(value)
  if isempty(value)
    colors = [0.066 0.443 0.745];
  elseif ischar(value) || isStringScalar(value)
    colors = validatecolor(value);
  elseif isstring(value) || iscellstr(value)
    names = cellstr(value(:));
    colors = zeros(numel(names), 3);
    for k = 1:numel(names)
      colors(k, :) = validatecolor(names{k});
    end
  elseif isnumeric(value) && size(value, 2) == 3
    colors = double(value);
  else
    error('Nelson:scatterhistogram:invalidColor', '%s must be a color value.', 'Color');
  end
end
%=============================================================================
function colors = colorCycle(colors, count)
  if isempty(colors)
    colors = [0.066 0.443 0.745];
  end
  if size(colors, 1) < count
    colors = repmat(colors, ceil(count / size(colors, 1)), 1);
  end
  colors = colors(1:count, :);
end
%=============================================================================
function value = firstColor(colors)
  if isempty(colors)
    value = [0.066 0.443 0.745];
  else
    value = colors(1, :);
  end
end
%=============================================================================
function value = markerMatchedColor(color)
  % The SVG export rounds marker colours but truncates stroke colours, so a
  % marginal histogram stroke set to the exact series colour renders up to one
  % 8-bit level darker per channel than the scatter markers of the same colour.
  % Snapping each channel to the centre of its 8-bit cell makes the truncated
  % stroke land on the same byte the rounded markers use, keeping the marginal
  % histograms colour-matched to the scatter series.
  value = color;
  if ~isnumeric(value) || numel(value) < 3
    return
  end
  value = double(value);
  value(:) = min(1, max(0, (round(value(:) * 255) + 0.5) / 255));
end
%=============================================================================
function value = firstCellValue(values)
  value = values{1};
end
%=============================================================================
function value = firstNumericValue(values)
  value = values(1);
end
%=============================================================================
function value = cycledCellValue(values, index)
  value = values{1 + mod(index - 1, numel(values))};
end
%=============================================================================
function value = cycledNumericValue(values, index)
  value = values(1 + mod(index - 1, numel(values)));
end
%=============================================================================
function value = cycledNumBins(numBins, index, horizontal)
  row = 1 + horizontal;
  if size(numBins, 2) == 1
    value = numBins(row);
  else
    value = numBins(row, 1 + mod(index - 1, size(numBins, 2)));
  end
end
%=============================================================================
function value = canonicalChoice(value, allowed)
  lowerValue = lower(value);
  idx = find(strcmp(lowerValue, allowed), 1);
  if strcmp(allowed{idx}, 'southwest'), value = 'SouthWest'; end
  if strcmp(allowed{idx}, 'southeast'), value = 'SouthEast'; end
  if strcmp(allowed{idx}, 'northeast'), value = 'NorthEast'; end
  if strcmp(allowed{idx}, 'northwest'), value = 'NorthWest'; end
end
%=============================================================================
function value = ternary(condition, trueValue, falseValue)
  if condition
    value = trueValue;
  else
    value = falseValue;
  end
end
%=============================================================================
