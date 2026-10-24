%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = parallelplot(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  [data, labels, sourceTable, coordinateVariables, args] = parseParallelData(args);
  opts = parseParallelOptions(args, labels, sourceTable);
  [normalized, limits] = normalizeParallelData(data, opts.DataNormalization);
  ax = createParallelAxes(parent);
  chartArgs = parallelPlotProperties(data, sourceTable, coordinateVariables, opts);
  group = __parallelplot__('Parent', ax, chartArgs{:});
  [axisHandles, labelHandles, tickHandles, baselineHandle] = drawCoordinateAxes(group, opts.CoordinateTickLabels, limits);
  dataHandles = drawParallelRows(group, normalized, opts);
  legendHandles = drawParallelLegend(group, dataHandles, opts, size(data, 2));
  applyParallelAxesState(ax, size(data, 2), ~isempty(legendHandles));
  state = struct();
  state.ChartType = 'parallelplot';
  state.Data = data;
  state.NormalizedData = normalized;
  state.DataLimits = limits;
  state.CoordinateTickLabels = opts.CoordinateTickLabels;
  state.GroupData = opts.GroupData;
  state.DataNormalization = opts.DataNormalization;
  state.DataHandles = dataHandles;
  state.AxisHandles = axisHandles;
  state.BaselineHandle = baselineHandle;
  state.LabelHandles = labelHandles;
  state.TickHandles = tickHandles;
  state.LegendHandles = legendHandles;
  group.UserData = state;
  if nargout == 1
    varargout{1} = group;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    error('Nelson:parallelplot:parallelCoordinatesPlotCannotBeA', 'ParallelCoordinatesPlot cannot be a child of Axes.');
  end
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'figure')
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [data, labels, sourceTable, coordinateVariables, args] = parseParallelData(args)
  if isempty(args)
    error('Nelson:graphics:dataExpected', 'Data expected.');
  end
  source = args{1};
  args = args(2:end);
  labels = {};
  sourceTable = [];
  coordinateVariables = {};
  if istable(source) || istimetable(source)
    sourceTable = source;
    variables = {};
    if ~isempty(args) && isTableVariableSelector(args{1}) && ~isParallelPropertyName(args{1})
      error('Nelson:parallelplot:specifyTheGroupVariableUsing', 'Specify the ''GroupVariable'' using a property name and value pair.');
    end
    [variables, args] = extractCoordinateVariables(args, variables);
    coordinateVariables = variables;
    [data, labels] = tableData(source, variables);
  elseif isnumeric(source) || islogical(source)
    data = double(source);
    [data, labels, args] = extractCoordinateData(data, args);
    if isempty(labels)
      labels = defaultLabels(size(data, 2));
    end
  else
    error('Nelson:parallelplot:numericMatrixOrExpected', 'Numeric matrix or table expected.');
  end
  if ~ismatrix(data) || isempty(data)
    error('Nelson:parallelplot:invalidData', 'Data must be a nonempty matrix.');
  end
end
%=============================================================================
function opts = parseParallelOptions(args, labels, sourceTable)
  opts = struct('CoordinateTickLabels', {labels}, 'GroupData', [], ...
    'GroupVariable', [], ...
    'DataNormalization', 'range', 'LineWidth', 1, 'LineStyle', '-', ...
    'LineAlpha', 0.7, 'MarkerStyle', 'none', 'MarkerSize', 6, 'Jitter', 0.1, ...
    'LegendTitle', '', 'CoordinateLabel', '', 'DataLabel', '', ...
    'LegendVisible', 'off', 'Title', '', 'FontName', 'Helvetica', 'FontSize', 10, ...
    'Color', []);
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'coordinateticklabels'
        opts.CoordinateTickLabels = normalizeLabels(value, numel(labels));
      case 'groupdata'
        opts.GroupData = value(:);
      case 'groupvariable'
        if isempty(sourceTable)
          error('Nelson:parallelplot:groupVariableIsSupportedOnly', 'GroupVariable is supported only for table input.');
        end
        [groupValues, groupName] = graphicsTableVariable(sourceTable, value);
        opts.GroupVariable = groupName;
        opts.GroupData = groupValues(:);
      case 'datanormalization'
        opts.DataNormalization = validateNormalization(value);
      case 'linewidth'
        opts.LineWidth = validateLineWidth(value);
      case 'linestyle'
        opts.LineStyle = convertTextValue(value);
      case 'linealpha'
        opts.LineAlpha = validateScalar(value, 'LineAlpha');
      case 'markerstyle'
        opts.MarkerStyle = convertTextValue(value);
      case 'markersize'
        opts.MarkerSize = validateScalar(value, 'MarkerSize');
      case 'jitter'
        opts.Jitter = validateScalar(value, 'Jitter');
      case 'legendtitle'
        opts.LegendTitle = convertTextValue(value);
      case 'coordinatelabel'
        opts.CoordinateLabel = convertTextValue(value);
      case 'datalabel'
        opts.DataLabel = convertTextValue(value);
      case 'legendvisible'
        opts.LegendVisible = validateOnOff(value);
      case 'title'
        opts.Title = convertTextValue(value);
      case 'fontname'
        opts.FontName = convertTextValue(value);
      case 'fontsize'
        opts.FontSize = validateScalar(value, 'FontSize');
      case 'color'
        opts.Color = value;
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'parallelplot');
    end
  end
end
%=============================================================================
function ax = createParallelAxes(parent)
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = newplot(parent);
  end
end
%=============================================================================
function args = parallelPlotProperties(data, sourceTable, coordinateVariables, opts)
  args = {'Data', data, ...
    'GroupData', opts.GroupData, ...
    'CoordinateData', data, ...
    'Color', plotColor(opts), ...
    'FontSize', opts.FontSize, ...
    'CoordinateTickLabels', opts.CoordinateTickLabels, ...
    'DataNormalization', opts.DataNormalization, ...
    'LineWidth', opts.LineWidth, ...
    'LineStyle', opts.LineStyle, ...
    'LineAlpha', opts.LineAlpha, ...
    'MarkerStyle', opts.MarkerStyle, ...
    'MarkerSize', opts.MarkerSize, ...
    'Jitter', opts.Jitter, ...
    'LegendTitle', opts.LegendTitle, ...
    'CoordinateLabel', opts.CoordinateLabel, ...
    'DataLabel', opts.DataLabel, ...
    'LegendVisible', opts.LegendVisible, ...
    'Title', opts.Title, ...
    'FontName', opts.FontName};
  if ~isempty(sourceTable)
    args = [args, {'SourceTable', sourceTable}];
  end
  if ~isempty(coordinateVariables)
    args = [args, {'CoordinateVariables', coordinateVariables}];
  end
  if ~isempty(opts.GroupVariable)
    args = [args, {'GroupVariable', opts.GroupVariable}];
  end
end
%=============================================================================
function [normalized, limits] = normalizeParallelData(data, method)
  limits = [min(data, [], 1); max(data, [], 1)];
  normalized = data;
  if strcmp(method, 'none')
    return
  end
  span = limits(2, :) - limits(1, :);
  span(span == 0) = 1;
  normalized = (data - repmat(limits(1, :), size(data, 1), 1)) ./ repmat(span, size(data, 1), 1);
end
%=============================================================================
function [axisHandles, labelHandles, tickHandles, baselineHandle] = drawCoordinateAxes(parent, labels, limits)
  n = numel(labels);
  axisHandles = gobjects(1, n);
  labelHandles = gobjects(1, n);
  tickHandles = gobjects(0, 1);
  axesColor = [0.25 0.25 0.25];
  baselineHandle = line('Parent', parent, 'XData', [0.5, n + 0.5], ...
    'YData', [-0.03, -0.03], 'Color', axesColor, 'LineWidth', 1, 'Clipping', 'off');
  for k = 1:n
    axisHandles(k) = line('Parent', parent, 'XData', [k k], 'YData', [0 1], ...
      'Color', axesColor, 'LineWidth', 1);
    tickHandles = [tickHandles; drawCoordinateTicks(parent, k, n, limits(:, k), axesColor)];
    labelHandles(k) = __text__('Parent', parent, 'Position', [k -0.08 0], 'String', labels{k}, ...
      'HorizontalAlignment', 'center', 'VerticalAlignment', 'top');
  end
end
%=============================================================================
function handles = drawCoordinateTicks(parent, coordinateIndex, coordinateCount, limits, axesColor)
  ticks = coordinateTicks(limits);
  handles = gobjects(0, 1);
  span = limits(2) - limits(1);
  if span == 0
    span = 1;
  end
  for k = 1:numel(ticks)
    y = (ticks(k) - limits(1)) / span;
    if coordinateIndex == coordinateCount
      tickX = [coordinateIndex - 0.03, coordinateIndex];
    else
      tickX = [coordinateIndex, coordinateIndex + 0.03];
    end
    handles(end + 1, 1) = line('Parent', parent, ...
      'XData', tickX, 'YData', [y y], 'Color', axesColor, 'LineWidth', 0.5);
    if coordinateIndex == coordinateCount
      labelX = coordinateIndex + 0.06;
      labelAlignment = 'left';
    else
      labelX = coordinateIndex - 0.06;
      labelAlignment = 'right';
    end
    handles(end + 1, 1) = __text__('Parent', parent, ...
      'Position', [labelX, y, 0], 'String', tickLabel(ticks(k)), ...
      'HorizontalAlignment', labelAlignment, 'VerticalAlignment', 'middle');
  end
end
%=============================================================================
function ticks = coordinateTicks(limits)
  span = limits(2) - limits(1);
  if ~isfinite(span) || span <= 0
    ticks = limits(1);
    return
  end
  rawStep = span / 10;
  magnitude = 10 ^ floor(log10(rawStep));
  scaled = rawStep / magnitude;
  if scaled <= 1
    multiplier = 1;
  elseif scaled <= 2
    multiplier = 2;
  elseif scaled <= 5
    multiplier = 5;
  else
    multiplier = 10;
  end
  step = multiplier * magnitude;
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  ticks = first:step:last;
end
%=============================================================================
function label = tickLabel(value)
  if abs(value - round(value)) < 1e-12
    label = sprintf('%d', round(value));
  else
    label = sprintf('%g', value);
  end
end
%=============================================================================
function dataHandles = drawParallelRows(parent, normalized, opts)
  n = size(normalized, 1);
  dataHandles = gobjects(n, 1);
  colors = colorsForRows(ancestor(parent, 'axes'), opts, n);
  x = 1:size(normalized, 2);
  for k = 1:n
    dataHandles(k) = line('Parent', parent, 'XData', x, 'YData', normalized(k, :), ...
      'Color', colors(k, :), 'LineWidth', opts.LineWidth, 'LineStyle', opts.LineStyle);
  end
end
%=============================================================================
function colors = colorsForRows(ax, opts, n)
  if ~isempty(opts.Color)
    colors = repmat(validatecolor(opts.Color), n, 1);
    return
  end
  order = ax.ColorOrder;
  colors = zeros(n, 3);
  groupIndex = groupIndices(opts.GroupData, n);
  for k = 1:n
    colors(k, :) = order(1 + mod(groupIndex(k) - 1, size(order, 1)), :);
  end
end
%=============================================================================
function legendHandles = drawParallelLegend(parent, dataHandles, opts, coordinateCount)
  legendHandles = gobjects(0, 1);
  if isempty(opts.GroupData) || strcmp(opts.LegendVisible, 'off')
    return
  end
  [levels, labels] = groupLevelsAndLabels(opts.GroupData);
  if numel(levels) <= 1
    return
  end
  x0 = coordinateCount + 0.28;
  x1 = coordinateCount + 0.5;
  yTop = 1.05;
  rowStep = 0.045;
  yBottom = yTop - rowStep * (numel(levels) + 1);
  legendHandles(end + 1, 1) = line('Parent', parent, ...
    'XData', [x0 x1 x1 x0 x0], 'YData', [yBottom yBottom yTop yTop yBottom], ...
    'Color', [0.25 0.25 0.25], 'LineWidth', 1, 'Clipping', 'off');
  titleText = opts.LegendTitle;
  if isempty(titleText)
    titleText = opts.GroupVariable;
  end
  if ~isempty(titleText)
    legendHandles(end + 1, 1) = __text__('Parent', parent, ...
      'Position', [(x0 + x1) / 2, yTop - rowStep / 2, 0], 'String', titleText, ...
      'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', ...
      'FontWeight', 'bold', 'Clipping', 'off');
  end
  for k = 1:numel(levels)
    sourceHandle = dataHandles(firstGroupIndex(opts.GroupData, levels{k}));
    y = yTop - rowStep * (k + 0.5);
    legendHandles(end + 1, 1) = line('Parent', parent, 'XData', [x0 + 0.04, x0 + 0.13], ...
      'YData', [y y], ...
      'Color', sourceHandle.Color, 'LineWidth', sourceHandle.LineWidth, ...
      'LineStyle', sourceHandle.LineStyle, 'Clipping', 'off');
    legendHandles(end + 1, 1) = __text__('Parent', parent, ...
      'Position', [x0 + 0.15, y, 0], 'String', labels{k}, ...
      'HorizontalAlignment', 'left', 'VerticalAlignment', 'middle', ...
      'Clipping', 'off');
  end
end
%=============================================================================
function idx = firstGroupIndex(groupData, level)
  idx = 1;
  for k = 1:numel(groupData)
    if isequal(groupData(k), level)
      idx = k;
      return
    end
  end
end
%=============================================================================
function [levels, labels] = groupLevelsAndLabels(groupData)
  levels = {};
  labels = {};
  for k = 1:numel(groupData)
    value = groupData(k);
    found = false;
    for j = 1:numel(levels)
      if isequal(value, levels{j})
        found = true;
      end
    end
    if ~found
      levels{end + 1} = value;
      labels{end + 1} = groupLabel(value);
    end
  end
end
%=============================================================================
function label = groupLabel(value)
  if iscell(value)
    label = value{1};
  elseif isstring(value)
    label = char(value);
  elseif ischar(value)
    label = value;
  elseif isnumeric(value) || islogical(value)
    label = num2str(value);
  else
    label = char(string(value));
  end
end
%=============================================================================
function color = plotColor(opts)
  if isempty(opts.Color)
    color = [0.066 0.443 0.745];
  else
    color = validatecolor(opts.Color);
  end
end
%=============================================================================
function index = groupIndices(groupData, n)
  index = ones(n, 1);
  if isempty(groupData)
    return
  end
  if numel(groupData) ~= n
    error('Nelson:parallelplot:invalidGroupData', 'GroupData must have one element per row.');
  end
  levels = {};
  for k = 1:n
    found = 0;
    for j = 1:numel(levels)
      if isequal(groupData(k), levels{j})
        found = j;
      end
    end
    if found == 0
      levels{end + 1} = groupData(k);
      found = numel(levels);
    end
    index(k) = found;
  end
end
%=============================================================================
function applyParallelAxesState(ax, n, hasLegend)
  ax.XLim = [0.5, n + 0.5];
  ax.YLim = [-0.04, 1.05];
  ax.XTick = 1:n;
  ax.XTickLabel = {};
  ax.YTick = [];
  ax.XColor = 'none';
  ax.YColor = 'none';
  ax.Visible = 'off';
  ax.Box = 'off';
end
%=============================================================================
function tf = isTableVariableSelector(value)
  tf = ischar(value) || iscellstr(value) || isstring(value);
  tf = tf && ~isempty(value);
end
%=============================================================================
function tf = isParallelPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  names = {'CoordinateVariables', 'CoordinateData', 'CoordinateTickLabels', ...
    'GroupData', 'GroupVariable', 'DataNormalization', 'LineWidth', ...
    'LineStyle', 'LineAlpha', 'MarkerStyle', 'MarkerSize', 'Jitter', ...
    'LegendTitle', 'CoordinateLabel', 'DataLabel', 'LegendVisible', ...
    'Title', 'FontName', 'FontSize', 'Color'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
function [variables, args] = extractCoordinateVariables(args, variables)
  index = findPropertyIndex(args, 'CoordinateVariables');
  if index == 0
    return
  end
  variables = normalizeVariableNames(args{index + 1});
  args(index:index + 1) = [];
end
%=============================================================================
function [data, labels, args] = extractCoordinateData(data, args)
  labels = {};
  index = findPropertyIndex(args, 'CoordinateData');
  if index == 0
    return
  end
  columns = args{index + 1};
  if ~isnumeric(columns) && ~islogical(columns)
    error('Nelson:parallelplot:invalidCoordinateData', '%s must be a numeric vector.', 'CoordinateData');
  end
  columns = columns(:)';
  data = data(:, columns);
  labels = defaultLabels(size(data, 2));
  for k = 1:numel(columns)
    labels{k} = num2str(columns(k));
  end
  args(index:index + 1) = [];
end
%=============================================================================
function index = findPropertyIndex(args, name)
  index = 0;
  if rem(length(args), 2) ~= 0
    return
  end
  for k = 1:2:length(args)
    if ischar(args{k}) && strcmpi(args{k}, name)
      index = k;
    end
  end
end
%=============================================================================
function names = normalizeVariableNames(names)
  if ischar(names)
    names = {names};
  elseif isstring(names)
    names = cellstr(names(:)');
  elseif iscellstr(names)
    names = names(:)';
  else
    error('Nelson:graphics:invalidVariableNames', 'Variable names must be character vectors or strings.');
  end
end
%=============================================================================
function [data, labels] = tableData(tbl, variables)
  explicitVariables = ~isempty(variables);
  if isempty(variables)
    variables = tbl.Properties.VariableNames;
  end
  labels = {};
  data = [];
  for k = 1:numel(variables)
    name = variables{k};
    if ~any(strcmp(name, tbl.Properties.VariableNames))
      error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
    end
    value = tbl.(name);
    if isnumeric(value) || islogical(value)
      [data, labels] = appendColumnData(data, labels, name, double(value), height(tbl));
    elseif explicitVariables
      error('Nelson:graphics:numericTableVariableExpected', 'Numeric table variable expected.');
    end
  end
  if isempty(data)
    error('Nelson:parallelplot:invalidTable', 'Table must contain numeric variables.');
  end
end
%=============================================================================
function [data, labels] = appendColumnData(data, labels, name, value, expectedRows)
  if isvector(value)
    value = value(:);
  end
  if size(value, 1) ~= expectedRows
    error('Nelson:graphics:invalidTableVariables', 'Table variables must have compatible row counts.');
  end
  if isempty(data)
    data = value;
  else
    data = [data, value];
  end
  if size(value, 2) == 1
    labels{end + 1} = name;
  else
    for k = 1:size(value, 2)
      labels{end + 1} = [name, '_', num2str(k)];
    end
  end
end
%=============================================================================
function labels = defaultLabels(n)
  labels = cell(1, n);
  for k = 1:n
    labels{k} = num2str(k);
  end
end
%=============================================================================
function labels = normalizeLabels(value, expectedCount)
  if isstring(value)
    labels = cellstr(value(:)');
  elseif ischar(value)
    labels = cellstr(value);
  elseif iscellstr(value)
    labels = value(:)';
  else
    error('Nelson:parallelplot:invalidCoordinateLabels', 'Coordinate labels must be text.');
  end
  if numel(labels) ~= expectedCount
    error('Nelson:parallelplot:invalidNumberOfCoordinate', 'Number of coordinate labels must match the number of variables.');
  end
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
function value = validateNormalization(value)
  value = lower(char(value));
  if ~(strcmp(value, 'range') || strcmp(value, 'none'))
    error('Nelson:parallelplot:invalidDataNormalization', 'DataNormalization must be ''range'' or ''none''.');
  end
end
%=============================================================================
function value = validateLineWidth(value)
  if ~isnumeric(value) || ~isscalar(value) || value <= 0
    error('Nelson:graphics:mustBePositiveScalar', '%s must be a positive scalar.', 'LineWidth');
  end
end
%=============================================================================
function value = validateScalar(value, name)
  if ~isnumeric(value) || ~isscalar(value)
    error('Nelson:graphics:mustBeNumericScalar', '%s must be a numeric scalar.', name);
  end
end
%=============================================================================
function value = validateOnOff(value)
  value = convertTextValue(value);
  if ~(strcmp(value, 'on') || strcmp(value, 'off'))
    error('Nelson:graphics:invalidOnOffValue', 'Value must be ''on'' or ''off''.');
  end
end
%=============================================================================
