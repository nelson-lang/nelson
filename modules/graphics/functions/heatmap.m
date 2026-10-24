%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = heatmap(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  [xLabels, yLabels, colorData, source, args] = parseHeatmapData(args);
  opts = defaultHeatmapOptions(source);
  [opts, imageArgs] = parseHeatmapOptions(opts, args);

  [rows, cols] = size(colorData);
  if ~isempty(opts.XData)
    xLabels = opts.XData;
  end
  if ~isempty(opts.YData)
    yLabels = opts.YData;
  end
  if ~isempty(opts.XDisplayLabels)
    xLabels = opts.XDisplayLabels;
  end
  if ~isempty(opts.YDisplayLabels)
    yLabels = opts.YDisplayLabels;
  end
  xLabels = labelsForAxis(xLabels, cols);
  yLabels = labelsForAxis(yLabels, rows);
  opts.ColorLimits = colorLimitsForChart(colorData, opts.ColorLimits);
  chartArgs = heatmapChartProperties(xLabels, yLabels, colorData, opts);
  [ax, group] = createHeatmapGroup(parent, chartArgs);

  applyHeatmapColorScale(ax, opts);
  % The chart draws its image, grid, missing-data overlay and cell labels
  % as internal primitives. They are created with HandleVisibility 'off' so
  % the heatmap behaves as an opaque chart: findobj/get(...,'Children') do
  % not expose them to external traversal, while they still render and stay
  % reachable through the returned handles stored in UserData.
  imageHandle = __image__('Parent', group, 'CData', colorData, ...
    'XData', [1, max(cols, 1)], 'YData', [1, max(rows, 1)], ...
    'CDataMapping', 'scaled', 'HandleVisibility', 'off', imageArgs{:});
  applyAxesState(ax, rows, cols, xLabels, yLabels, opts);
  missingHandles = drawMissingDataCells(group, colorData, opts);
  gridHandles = drawHeatmapGrid(group, rows, cols, opts);
  labelHandles = drawCellLabels(group, colorData, opts);
  colorbarHandle = createHeatmapColorbar(ax, opts);
  applyTextLabels(ax, opts);

  state = struct();
  state.ChartType = 'heatmap';
  state.Image = imageHandle;
  state.MissingCells = missingHandles;
  state.Grid = gridHandles;
  state.CellLabels = labelHandles;
  state.Colorbar = colorbarHandle;
  state.XData = xLabels;
  state.YData = yLabels;
  state.ColorData = colorData;
  state.SourceTable = opts.SourceTable;
  state.XVariable = opts.XVariable;
  state.YVariable = opts.YVariable;
  state.ColorVariable = opts.ColorVariable;
  state.ColorMethod = opts.ColorMethod;
  state.Options = opts;
  group.UserData = state;

  if nargout == 1
    varargout{1} = group;
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
function [xLabels, yLabels, colorData, source, args] = parseHeatmapData(args)
  if isempty(args)
    error('Nelson:heatmap:colorDataExpected', 'Color data expected.');
  end
  xLabels = [];
  yLabels = [];
  source = emptyHeatmapSource();
  if isHeatmapTable(args{1})
    if length(args) < 3
      error('Nelson:heatmap:tableXVariableExpected', 'Table, x variable, and y variable expected.');
    end
    tbl = args{1};
    [xValues, xName] = graphicsTableVariable(tbl, args{2});
    [yValues, yName] = graphicsTableVariable(tbl, args{3});
    args = args(4:end);
    [colorValues, colorName] = tableColorVariable(tbl, args);
    [xLabels, yLabels, colorData] = heatmapDataFromTable(xValues, yValues, colorValues);
    source.Table = tbl;
    source.XVariable = xName;
    source.YVariable = yName;
    source.ColorVariable = colorName;
    if isempty(colorName)
      source.ColorMethod = 'count';
    else
      source.ColorMethod = 'mean';
    end
  elseif isnumeric(args{1})
    colorData = args{1};
    args = args(2:end);
  elseif length(args) >= 3 && isnumeric(args{3})
    xLabels = args{1};
    yLabels = args{2};
    colorData = args{3};
    args = args(4:end);
  else
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Color data');
  end
  if ~ismatrix(colorData) || ~isnumeric(colorData)
    error('Nelson:heatmap:invalidColorData', '%s must be a numeric matrix.', 'Color data');
  end
end
%=============================================================================
function source = emptyHeatmapSource()
  source = struct();
  source.Table = [];
  source.XVariable = '';
  source.YVariable = '';
  source.ColorVariable = '';
  source.ColorMethod = 'none';
end
%=============================================================================
function tf = isHeatmapTable(value)
  tf = istable(value);
  if ~tf && exist('istimetable', 'file')
    tf = istimetable(value);
  end
end
%=============================================================================
function [colorValues, colorName] = tableColorVariable(tbl, args)
  colorValues = [];
  colorName = '';
  if isempty(args)
    return
  end
  for k = 1:2:length(args) - 1
    name = args{k};
    if (ischar(name) || isStringScalar(name)) && strcmpi(convertStringsToChars(name), 'ColorVariable')
      [colorValues, colorName] = graphicsTableVariable(tbl, args{k + 1});
    end
  end
  if ~isempty(colorName)
    if ~isnumeric(colorValues) || ~isvector(colorValues)
      error('Nelson:heatmap:colorVariableNotNumeric', 'ColorVariable must select a numeric vector.');
    end
    colorValues = double(colorValues(:));
  end
end
%=============================================================================
function [xLabels, yLabels, colorData] = heatmapDataFromTable(xValues, yValues, colorValues)
  if numel(xValues) ~= numel(yValues)
    error('Nelson:heatmap:invalidXAndY', 'X and Y table variables must have the same number of rows.');
  end
  xAll = graphicsLabelsFromValue(xValues, numel(xValues));
  yAll = graphicsLabelsFromValue(yValues, numel(yValues));
  xLabels = unique(xAll(:));
  yLabels = unique(yAll(:));
  if isempty(colorValues)
    colorData = aggregateCount(xAll, yAll, xLabels, yLabels);
  else
    if numel(colorValues) ~= numel(xAll)
      error('Nelson:heatmap:invalidColorVariable', 'ColorVariable must have the same number of rows as XVariable and YVariable.');
    end
    colorData = aggregateMean(xAll, yAll, colorValues, xLabels, yLabels);
  end
end
%=============================================================================
function colorData = aggregateCount(xAll, yAll, xLabels, yLabels)
  colorData = zeros(numel(yLabels), numel(xLabels));
  for k = 1:numel(xAll)
    col = find(strcmp(xLabels, xAll{k}), 1);
    row = find(strcmp(yLabels, yAll{k}), 1);
    colorData(row, col) = colorData(row, col) + 1;
  end
end
%=============================================================================
function colorData = aggregateMean(xAll, yAll, colorValues, xLabels, yLabels)
  sums = zeros(numel(yLabels), numel(xLabels));
  counts = zeros(numel(yLabels), numel(xLabels));
  for k = 1:numel(xAll)
    col = find(strcmp(xLabels, xAll{k}), 1);
    row = find(strcmp(yLabels, yAll{k}), 1);
    sums(row, col) = sums(row, col) + colorValues(k);
    counts(row, col) = counts(row, col) + 1;
  end
  colorData = sums ./ counts;
  colorData(counts == 0) = NaN;
end
%=============================================================================
function opts = defaultHeatmapOptions(source)
  opts = struct();
  [defaultTitle, defaultXLabel, defaultYLabel] = defaultTableLabels(source);
  opts.Title = defaultTitle;
  opts.XLabel = defaultXLabel;
  opts.YLabel = defaultYLabel;
  opts.SourceTable = source.Table;
  opts.XVariable = source.XVariable;
  opts.YVariable = source.YVariable;
  opts.ColorVariable = source.ColorVariable;
  opts.ColorMethod = source.ColorMethod;
  opts.XData = [];
  opts.YData = [];
  opts.XDisplayLabels = [];
  opts.YDisplayLabels = [];
  opts.ColorLimits = [];
  opts.Colormap = defaultHeatmapColormap();
  opts.ColorbarVisible = 'on';
  opts.GridVisible = 'on';
  opts.CellLabelFormat = '%0.4g';
  opts.CellLabelColor = 'auto';
  opts.MissingDataLabel = 'NaN';
  opts.MissingDataColor = [0.1294 0.1294 0.1294];
  opts.FontColor = [0.1294 0.1294 0.1294];
  opts.FontSize = 10;
  opts.Visible = 'on';
end
%=============================================================================
function [titleText, xLabel, yLabel] = defaultTableLabels(source)
  titleText = '';
  xLabel = '';
  yLabel = '';
  if isempty(source.Table)
    return
  end
  xLabel = source.XVariable;
  yLabel = source.YVariable;
  if isempty(source.ColorVariable)
    titleText = ['Count of ', yLabel, ' vs. ', xLabel];
  else
    titleText = ['Mean of ', source.ColorVariable];
  end
end
%=============================================================================
function cm = defaultHeatmapColormap()
  firstColor = [0.9066 0.9443 0.9745];
  lastColor = [0.066 0.443 0.745];
  steps = linspace(0, 1, 256)';
  cm = firstColor + steps * (lastColor - firstColor);
end
%=============================================================================
function [opts, imageArgs] = parseHeatmapOptions(opts, args)
  imageArgs = {};
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = args{k};
    value = args{k + 1};
    if ~(ischar(name) || isStringScalar(name))
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Property name');
    end
    name = lower(convertStringsToChars(name));
    switch name
      case 'title'
        opts.Title = convertTextValue(value);
      case 'xlabel'
        opts.XLabel = convertTextValue(value);
      case 'ylabel'
        opts.YLabel = convertTextValue(value);
      case 'sourcetable'
        opts.SourceTable = value;
      case 'xvariable'
        opts.XVariable = convertTextValue(value);
      case 'yvariable'
        opts.YVariable = convertTextValue(value);
      case 'colorvariable'
        opts.ColorVariable = convertTextValue(value);
      case 'colormethod'
        opts.ColorMethod = convertTextValue(value);
      case 'xdata'
        opts.XData = value;
      case 'ydata'
        opts.YData = value;
      case 'xdisplaylabels'
        opts.XDisplayLabels = value;
      case 'ydisplaylabels'
        opts.YDisplayLabels = value;
      case 'colordata'
        error('Nelson:heatmap:colorDataPositional', 'ColorData must be specified as a positional argument.');
      case 'colorlimits'
        opts.ColorLimits = validateColorLimits(value);
      case 'colormap'
        opts.Colormap = value;
      case 'colorbarvisible'
        opts.ColorbarVisible = validateOnOff(value);
      case 'gridvisible'
        opts.GridVisible = validateOnOff(value);
      case 'celllabelformat'
        opts.CellLabelFormat = convertTextValue(value);
      case 'celllabelcolor'
        opts.CellLabelColor = value;
      case 'missingdatalabel'
        opts.MissingDataLabel = convertTextValue(value);
      case 'missingdatacolor'
        opts.MissingDataColor = validateRgbColor(value, 'MissingDataColor');
      case 'fontcolor'
        opts.FontColor = validateRgbColor(value, 'FontColor');
      case 'fontsize'
        opts.FontSize = value;
      case 'visible'
        opts.Visible = validateOnOff(value);
      otherwise
        imageArgs = [imageArgs, {args{k}, value}];
    end
  end
end
%=============================================================================
function value = validateRgbColor(value, name)
  if ~isnumeric(value) || numel(value) ~= 3
    error('Nelson:heatmap:mustBeThreeElementVector', '%s must be a three-element numeric vector.', name);
  end
  value = double(reshape(value, 1, 3));
end
%=============================================================================
function value = convertTextValue(value)
  if isStringScalar(value)
    value = convertStringsToChars(value);
  end
  if ~ischar(value)
    error('Nelson:graphics:textValueExpected', 'Text value expected.');
  end
end
%=============================================================================
function limits = validateColorLimits(value)
  if ~isnumeric(value) || numel(value) ~= 2 || value(1) >= value(2)
    error('Nelson:heatmap:invalidColorLimits', 'ColorLimits must be a two-element increasing numeric vector.');
  end
  limits = value;
end
%=============================================================================
function value = validateOnOff(value)
  value = convertTextValue(value);
  if ~(strcmp(value, 'on') || strcmp(value, 'off'))
    error('Nelson:graphics:invalidOnOffValue', 'Value must be ''on'' or ''off''.');
  end
end
%=============================================================================
function [ax, group] = createHeatmapGroup(parent, chartArgs)
  if isempty(parent)
    ax = newplot();
    group = __heatmapchart__('Parent', ax, chartArgs{:});
  elseif isgraphics(parent, 'hggroup')
    group = __heatmapchart__('Parent', parent, chartArgs{:});
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
    group = __heatmapchart__('Parent', ax, chartArgs{:});
  end
end
%=============================================================================
function args = heatmapChartProperties(xLabels, yLabels, colorData, opts)
  args = {'ColorData', colorData, 'ColorDisplayData', colorData, ...
    'XData', xLabels, 'YData', yLabels, ...
    'XDisplayLabels', xLabels, 'YDisplayLabels', yLabels, ...
    'XDisplayData', xLabels, 'YDisplayData', yLabels, ...
    'Title', opts.Title, 'XLabel', opts.XLabel, 'YLabel', opts.YLabel, ...
    'SourceTable', opts.SourceTable, ...
    'XVariable', opts.XVariable, 'YVariable', opts.YVariable, ...
    'ColorVariable', opts.ColorVariable, 'ColorMethod', opts.ColorMethod, ...
    'ColorLimits', opts.ColorLimits, ...
    'ColorbarVisible', opts.ColorbarVisible, 'GridVisible', opts.GridVisible, ...
    'CellLabelFormat', opts.CellLabelFormat, 'CellLabelColor', opts.CellLabelColor, ...
    'MissingDataLabel', opts.MissingDataLabel, ...
    'FontColor', opts.FontColor, 'FontSize', opts.FontSize, 'Visible', opts.Visible};
  if ~isempty(opts.Colormap)
    args = [args, {'Colormap', opts.Colormap}];
  end
end
%=============================================================================
function limits = colorLimitsForChart(colorData, limits)
  if ~isempty(limits)
    return
  end
  finiteData = colorData(isfinite(colorData));
  if isempty(finiteData)
    limits = [0 1];
    return
  end
  limits = [min(finiteData(:)) max(finiteData(:))];
  if limits(1) == limits(2)
    limits(2) = limits(1) + 1;
  end
end
%=============================================================================
function labels = labelsForAxis(values, count)
  if isempty(values)
    labels = cell(count, 1);
    for k = 1:count
      labels{k} = num2str(k);
    end
    return
  end
  if iscellstr(values)
    labels = values(:);
  elseif isstring(values)
    labels = cellstr(values(:));
  elseif ischar(values)
    labels = cellstr(values);
  elseif isnumeric(values) || islogical(values)
    values = values(:);
    labels = cell(numel(values), 1);
    for k = 1:numel(values)
      labels{k} = num2str(values(k));
    end
  else
    error('Nelson:heatmap:invalidAxisLabels', 'Axis labels must be character, string, cell string, or numeric values.');
  end
  if numel(labels) ~= count
    error('Nelson:heatmap:invalidNumberOfAxis', 'Number of axis labels must match the color data size.');
  end
end
%=============================================================================
function applyAxesState(ax, rows, cols, xLabels, yLabels, opts)
  ax.YDir = 'reverse';
  ax.XLim = [0.5, cols + 0.5];
  ax.YLim = [0.5, rows + 0.5];
  ax.XTick = 1:cols;
  ax.YTick = 1:rows;
  ax.XTickLabel = xLabels;
  ax.YTickLabel = yLabels;
  ax.TickLength = [0 0];
  ax.Layer = 'top';
  ax.Box = 'on';
  ax.Visible = opts.Visible;
end
%=============================================================================
function handles = drawMissingDataCells(parent, colorData, opts)
  handles = [];
  [rows, cols] = size(colorData);
  mask = isnan(colorData);
  if ~any(mask(:))
    return
  end
  rgb = zeros(rows, cols, 3);
  rgb(:, :, 1) = opts.MissingDataColor(1);
  rgb(:, :, 2) = opts.MissingDataColor(2);
  rgb(:, :, 3) = opts.MissingDataColor(3);
  handles = __image__('Parent', parent, 'CData', rgb, ...
    'XData', [1, max(cols, 1)], 'YData', [1, max(rows, 1)], ...
    'CDataMapping', 'direct', 'AlphaData', double(mask), 'AlphaDataMapping', 'none', ...
    'HandleVisibility', 'off');
end
%=============================================================================
function handles = drawHeatmapGrid(parent, rows, cols, opts)
  handles = [];
  if strcmp(opts.GridVisible, 'off')
    return
  end
  color = graphicsDefaultEdgeColor();
  for k = 0:cols
    handles = [handles, line('Parent', parent, ...
      'XData', [k + 0.5, k + 0.5], 'YData', [0.5, rows + 0.5], ...
      'Color', color, 'LineWidth', 0.5, 'HandleVisibility', 'off')];
  end
  for k = 0:rows
    handles = [handles, line('Parent', parent, ...
      'XData', [0.5, cols + 0.5], 'YData', [k + 0.5, k + 0.5], ...
      'Color', color, 'LineWidth', 0.5, 'HandleVisibility', 'off')];
  end
end
%=============================================================================
function handles = drawCellLabels(parent, colorData, opts)
  handles = [];
  [rows, cols] = size(colorData);
  for i = 1:rows
    for j = 1:cols
      value = colorData(i, j);
      if isnan(value)
        label = opts.MissingDataLabel;
      else
        label = sprintf(opts.CellLabelFormat, value);
      end
      labelColor = cellLabelColor(value, opts);
      handles = [handles, __text__('Parent', parent, 'Position', [j i 0], 'String', label, ...
        'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', ...
        'FontSize', opts.FontSize, 'Color', labelColor, 'HandleVisibility', 'off')];
    end
  end
end
%=============================================================================
function color = cellLabelColor(value, opts)
  color = opts.CellLabelColor;
  if ~(ischar(color) && strcmp(color, 'auto'))
    return
  end
  color = opts.FontColor;
  if isnan(value) || isempty(opts.Colormap) || isempty(opts.ColorLimits)
    return
  end
  limits = opts.ColorLimits;
  position = (value - limits(1)) / (limits(2) - limits(1));
  position = max(0, min(1, position));
  index = round(position * (size(opts.Colormap, 1) - 1)) + 1;
  background = opts.Colormap(index, :);
  luminance = 0.2126 * background(1) + 0.7152 * background(2) + 0.0722 * background(3);
  if luminance < 0.5
    color = [1 1 1];
  end
end
%=============================================================================
function applyHeatmapColorScale(ax, opts)
  if ~isempty(opts.ColorLimits)
    clim(ax, opts.ColorLimits);
  end
  if ~isempty(opts.Colormap)
    colormap(ax, opts.Colormap);
  end
end
%=============================================================================
function colorbarHandle = createHeatmapColorbar(ax, opts)
  colorbarHandle = [];
  if strcmp(opts.ColorbarVisible, 'on')
    colorbarHandle = colorbar(ax);
  end
end
%=============================================================================
function applyTextLabels(ax, opts)
  if ~isempty(opts.Title)
    title(ax, opts.Title);
  end
  if ~isempty(opts.XLabel)
    xlabel(ax, opts.XLabel);
  end
  if ~isempty(opts.YLabel)
    ylabel(ax, opts.YLabel);
  end
end
%=============================================================================
