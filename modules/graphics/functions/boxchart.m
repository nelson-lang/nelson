%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = boxchart(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [posArgs, optionArgs] = splitDataAndOptions(args);
  [groupByColor, optionArgs] = extractGroupByColor(optionArgs);
  options = parseBoxchartOptions(optionArgs);
  [series, axesState] = parseBoxchartData(posArgs, groupByColor, options);
  [target, ax] = boxchartTarget(parent);
  handles = [];
  for k = 1:numel(series)
    ctorArgs = seriesConstructorArgs(target, series(k), options, k, numel(series));
    handles = [handles; __boxchart__(ctorArgs{:})];
  end
  applyAxesState(ax, axesState, options);
  if nargout == 1
    varargout{1} = handles;
  end
end
%=============================================================================
function [target, ax] = boxchartTarget(parent)
  if isempty(parent)
    ax = newplot();
    target = ax;
  elseif isgraphics(parent, 'hggroup')
    target = parent;
    ax = ancestor(parent, 'axes');
  else
    ax = newplot(parent);
    target = ax;
  end
end
%=============================================================================
function [posArgs, optionArgs] = splitDataAndOptions(args)
  firstOption = numel(args) + 1;
  for k = 1:numel(args)
    if isBoxchartOptionName(args{k})
      firstOption = k;
      break
    end
  end
  posArgs = args(1:firstOption - 1);
  optionArgs = args(firstOption:end);
  if rem(numel(optionArgs), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
end
%=============================================================================
function [groupByColor, args] = extractGroupByColor(args)
  groupByColor = [];
  kept = {};
  k = 1;
  while k <= numel(args)
    name = canonicalPropertyName(args{k});
    if strcmp(name, 'GroupByColor')
      groupByColor = args{k + 1};
    else
      kept(end + 1:end + 2) = args(k:k + 1);
    end
    k = k + 2;
  end
  args = kept;
end
%=============================================================================
function options = parseBoxchartOptions(args)
  options = struct();
  options.ConstructorArgs = {};
  options.BoxWidth = 0.5;
  options.ColorGroupWidth = 1;
  options.ColorGroupLayout = 'grouped';
  options.Orientation = 'vertical';
  k = 1;
  while k <= numel(args)
    name = canonicalPropertyName(args{k});
    value = args{k + 1};
    validateBoxchartOption(name, value);
    if strcmp(name, 'BoxWidth')
      options.BoxWidth = double(value);
    elseif strcmp(name, 'ColorGroupWidth')
      options.ColorGroupWidth = double(value);
    elseif strcmp(name, 'ColorGroupLayout')
      options.ColorGroupLayout = lower(char(value));
    elseif strcmp(name, 'Orientation')
      options.Orientation = lower(char(value));
    end
    options.ConstructorArgs(end + 1:end + 2) = {name, value};
    k = k + 2;
  end
end
%=============================================================================
function validateBoxchartOption(name, value)
  switch name
    case {'BoxWidth', 'CapWidth', 'LineWidth', 'MarkerSize'}
      validatePositiveScalar(value, name);
    case {'BoxFaceAlpha', 'ColorGroupWidth'}
      validateUnitScalar(value, name);
    case {'BoxFaceColor', 'BoxEdgeColor', 'BoxMedianLineColor', 'WhiskerLineColor', 'MarkerColor'}
      validateColorOrAuto(value, name);
    case {'Orientation'}
      validateMember(value, {'vertical', 'horizontal'}, name);
    case {'ColorGroupLayout'}
      validateMember(value, {'grouped', 'overlaid'}, name);
    case {'Notch', 'JitterOutliers', 'Clipping', 'Visible', 'HandleVisibility', ...
        'Selected', 'SelectionHighlight', 'HitTest', 'Interruptible'}
      validateOnOff(value, name);
    otherwise
  end
end
%=============================================================================
function [series, axesState] = parseBoxchartData(posArgs, groupByColor, options)
  if isempty(posArgs)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if istable(posArgs{1}) || istimetable(posArgs{1})
    [series, axesState] = parseTableBoxchartData(posArgs, groupByColor, options);
    return
  end
  [x, y, xLabels] = parseArrayBoxchartData(posArgs);
  [series, axesState] = buildSeriesFromArrays(x, y, xLabels, groupByColor, options);
end
%=============================================================================
function [x, y, xLabels] = parseArrayBoxchartData(posArgs)
  x = [];
  xLabels = [];
  if numel(posArgs) == 1
    y = posArgs{1};
  elseif numel(posArgs) == 2
    [x, xLabels] = groupPositions(posArgs{1}, numel(posArgs{2}));
    y = posArgs{2};
  else
    if containsTextArgument(posArgs(2:end))
      error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'boxchart');
    end
    error('Nelson:graphics:tooManyDataArguments', 'Too many data arguments.');
  end
  if ~isnumeric(y)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  % A single vector of values is one box group; the reference stores it (and
  % reads it back) as a row vector regardless of the input orientation.
  if isvector(y)
    y = reshape(y, 1, []);
  end
end
%=============================================================================
function tf = containsTextArgument(args)
  tf = false;
  for k = 1:numel(args)
    if ischar(args{k}) || isStringScalar(args{k})
      tf = true;
      return
    end
  end
end
%=============================================================================
function [series, axesState] = parseTableBoxchartData(posArgs, groupByColor, options)
  tbl = posArgs{1};
  if numel(posArgs) == 2
    yNames = graphicsTableVariableNames(tbl, posArgs{2});
    [y, labels] = tableNumericMatrix(tbl, yNames);
    base = makeSeries([], y, '', strjoin(yNames, ','), tbl);
    series = base;
    axesState = defaultAxesState(size(y, 2), labels);
  elseif numel(posArgs) == 3
    xNames = graphicsTableVariableNames(tbl, posArgs{2});
    yNames = graphicsTableVariableNames(tbl, posArgs{3});
    [series, axesState] = tableXYSeries(tbl, xNames, yNames, groupByColor, options);
    return
  else
    error('Nelson:graphics:tableVariableExpected', 'Table variable arguments expected.');
  end
  if ~isempty(groupByColor)
    groupByColor = tableGroupByColor(tbl, groupByColor, numel(y));
    [series, axesState] = applyGroupByColor(series, axesState, groupByColor, options);
  end
end
%=============================================================================
function [series, axesState] = tableXYSeries(tbl, xNames, yNames, groupByColor, options)
  rawGroupByColor = groupByColor;
  if numel(xNames) == 1 && numel(yNames) > 1
    series = repmat(emptySeries(), 1, numel(yNames));
    for k = 1:numel(yNames)
      [x, labels] = groupPositions(tbl.(xNames{1}), numel(tbl.(yNames{k})));
      series(k) = makeSeries(x, numericColumn(tbl.(yNames{k}), yNames{k}), xNames{1}, yNames{k}, tbl);
      series(k).DisplayName = yNames{k};
    end
    axesState = axesStateFromLabels(x, labels);
  elseif numel(xNames) == numel(yNames)
    series = repmat(emptySeries(), 1, numel(yNames));
    labels = [];
    x = [];
    for k = 1:numel(yNames)
      [x, labels] = groupPositions(tbl.(xNames{k}), numel(tbl.(yNames{k})));
      series(k) = makeSeries(x, numericColumn(tbl.(yNames{k}), yNames{k}), xNames{k}, yNames{k}, tbl);
      series(k).DisplayName = yNames{k};
    end
    axesState = axesStateFromLabels(x, labels);
  else
    error('Nelson:dimagree:tableVariableCountsMatch', 'Table variable counts must match.');
  end
  if ~isempty(groupByColor)
    groupByColor = tableGroupByColor(tbl, rawGroupByColor, numel(series(1).YData));
    [series, axesState] = applyGroupByColor(series, axesState, groupByColor, options);
  end
end
%=============================================================================
function [series, axesState] = buildSeriesFromArrays(x, y, xLabels, groupByColor, options)
  if isempty(groupByColor)
    series = makeSeries(x, y, '', '', []);
    if isempty(xLabels)
      axesState = defaultAxesStateForData(x, y);
    else
      axesState = axesStateFromLabels(x, xLabels);
    end
  else
    base = makeSeries(x, y, '', '', []);
    [series, axesState] = applyGroupByColor(base, axesStateFromLabels(x, xLabels), ...
      groupByColor, options);
  end
end
%=============================================================================
function s = emptySeries()
  s = struct('XData', [], 'YData', [], 'XVariable', '', 'YVariable', '', ...
    'SourceTable', [], 'DisplayName', '');
end
%=============================================================================
function s = makeSeries(x, y, xVariable, yVariable, sourceTable)
  s = emptySeries();
  s.XData = x;
  s.YData = y;
  s.XVariable = xVariable;
  s.YVariable = yVariable;
  s.SourceTable = sourceTable;
end
%=============================================================================
function ctorArgs = seriesConstructorArgs(target, s, options, index, count)
  ctorArgs = {'Parent', target, 'YData', s.YData};
  if ~isempty(s.XData)
    ctorArgs(end + 1:end + 2) = {'XData', s.XData};
  end
  if ~isempty(s.SourceTable)
    ctorArgs(end + 1:end + 2) = {'SourceTable', s.SourceTable};
  end
  if ~isempty(s.XVariable)
    ctorArgs(end + 1:end + 2) = {'XVariable', s.XVariable};
  end
  if ~isempty(s.YVariable)
    ctorArgs(end + 1:end + 2) = {'YVariable', s.YVariable};
  end
  if ~isempty(s.DisplayName)
    ctorArgs(end + 1:end + 2) = {'DisplayName', s.DisplayName};
  end
  if count > 1
    ctorArgs(end + 1:end + 4) = {'SeriesIndex', index, 'NumPeers', count};
  end
  ctorArgs = [ctorArgs, options.ConstructorArgs];
end
%=============================================================================
function [series, axesState] = applyGroupByColor(baseSeries, axesState, groupByColor, options)
  if numel(baseSeries) ~= 1
    error('Nelson:boxchart:groupByColorSupportsOneData', 'GroupByColor supports one data series at a time.');
  end
  y = baseSeries.YData;
  if ~isvector(y)
    error('Nelson:boxchart:groupByColorRequiresVectorData', 'GroupByColor requires vector data.');
  end
  y = y(:);
  if isempty(baseSeries.XData)
    x = ones(size(y));
    if isempty(axesState.Ticks)
      axesState = axesStateFromLabels(1, {'1'});
    end
  else
    x = baseSeries.XData(:);
  end
  if numel(x) ~= numel(y)
    error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'XData', 'YData');
  end
  labels = graphicsLabelsFromValue(groupByColor, numel(y));
  [colorLabels, colorIndex] = sortedUniqueLabels(labels);
  count = numel(colorLabels);
  series = repmat(emptySeries(), 1, count);
  for k = 1:count
    mask = colorIndex == k;
    sx = x(mask);
    series(k) = makeSeries(sx, y(mask), baseSeries.XVariable, baseSeries.YVariable, ...
      baseSeries.SourceTable);
    series(k).DisplayName = colorLabels{k};
  end
end
%=============================================================================
function offset = colorOffset(index, count, boxWidth, colorGroupWidth)
  if count <= 1
    offset = 0;
  else
    span = boxWidth * colorGroupWidth;
    offset = ((index - (count + 1) / 2) / count) * span;
  end
end
%=============================================================================
function [positions, labels] = groupPositions(value, expectedCount)
  labels = [];
  if isnumeric(value) || islogical(value)
    positions = double(value(:));
  else
    labels = graphicsLabelsFromValue(value, -1);
    if expectedCount >= 0 && numel(labels) ~= expectedCount
      error('Nelson:graphics:invalidGroupData', 'Group data must have the same number of elements as data.');
    end
    [uniqueLabels, index] = stableUniqueLabels(labels);
    positions = double(index(:));
    labels = uniqueLabels;
  end
  if expectedCount >= 0 && numel(positions) ~= expectedCount
    error('Nelson:graphics:invalidGroupData', 'Group data must have the same number of elements as data.');
  end
end
%=============================================================================
function [uniqueLabels, index] = stableUniqueLabels(labels)
  uniqueLabels = {};
  index = zeros(numel(labels), 1);
  for k = 1:numel(labels)
    found = find(strcmp(labels{k}, uniqueLabels), 1);
    if isempty(found)
      uniqueLabels{end + 1, 1} = labels{k};
      found = numel(uniqueLabels);
    end
    index(k) = found;
  end
end
%=============================================================================
function [uniqueLabels, index] = sortedUniqueLabels(labels)
  [uniqueLabels, ~] = stableUniqueLabels(labels);
  uniqueLabels = sort(uniqueLabels(:));
  index = zeros(numel(labels), 1);
  for k = 1:numel(labels)
    index(k) = find(strcmp(labels{k}, uniqueLabels), 1);
  end
end
%=============================================================================
function state = axesStateFromLabels(x, labels)
  state = struct('Ticks', [], 'Labels', [], 'Limits', []);
  if ~isempty(labels)
    state.Ticks = 1:numel(labels);
    state.Labels = labels(:);
    state.Limits = numericPositionLimits(state.Ticks);
  else
    [state.Limits, state.Ticks] = numericPositionLimits(x);
  end
end
%=============================================================================
function state = defaultAxesStateForData(x, y)
  if isempty(x) && ~isvector(y)
    state = defaultAxesState(size(y, 2), defaultLabels(size(y, 2)));
  elseif isempty(x)
    state = defaultAxesState(1, {'1'});
  else
    state = axesStateFromLabels(x, []);
  end
end
%=============================================================================
function state = defaultAxesState(count, labels)
  state = struct('Ticks', 1:count, 'Labels', {labels(:)}, 'Limits', defaultPositionLimits(count));
end
%=============================================================================
function limits = defaultPositionLimits(count)
  limits = [0.5 count + 0.5];
end
%=============================================================================
function [limits, ticks] = numericPositionLimits(values)
  limits = [];
  ticks = [];
  if isempty(values) || ~(isnumeric(values) || islogical(values))
    return
  end
  values = double(values(:));
  values = values(isfinite(values));
  if isempty(values)
    return
  end
  values = sort(unique(values(:).'));
  if numel(values) == 1
    step = 1;
  else
    steps = diff(values);
    steps = steps(steps > 0);
    if isempty(steps)
      step = 1;
    else
      step = min(steps);
    end
  end
  tickStep = step / 2;
  limits = [values(1) - tickStep, values(end) + tickStep];
  ticks = limits(1):tickStep:limits(2);
end
%=============================================================================
function applyAxesState(ax, state, options)
  if isempty(ax)
    return
  end
  if strcmp(options.Orientation, 'horizontal')
    if ~isempty(state.Ticks)
      ax.YTick = state.Ticks;
    end
    if ~isempty(state.Labels)
      ax.YTickLabel = state.Labels;
    end
    if ~isempty(state.Limits) && strcmp(ax.YLimMode, 'auto')
      ax.YLim = state.Limits;
    end
  else
    if ~isempty(state.Ticks)
      ax.XTick = state.Ticks;
    end
    if ~isempty(state.Labels)
      ax.XTickLabel = state.Labels;
    end
    if ~isempty(state.Limits) && strcmp(ax.XLimMode, 'auto')
      ax.XLim = state.Limits;
    end
  end
end
%=============================================================================
function [data, labels] = tableNumericMatrix(tbl, names)
  data = [];
  labels = {};
  for k = 1:numel(names)
    value = numericColumn(tbl.(names{k}), names{k});
    data = [data, value];
    labels{end + 1, 1} = names{k};
  end
end
%=============================================================================
function value = numericColumn(value, name)
  if ~isnumeric(value)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', name);
  end
  if ~isvector(value)
    error('Nelson:validators:mustBeVector', '%s must be a vector.', name);
  end
  value = double(value(:));
end
%=============================================================================
function value = tableGroupByColor(tbl, value, expectedCount)
  if ischar(value) || isStringScalar(value)
    try
      names = graphicsTableVariableNames(tbl, value);
      if numel(names) == 1 && numel(tbl.(names{1})) == expectedCount
        value = tbl.(names{1});
      end
    catch
    end
  end
end
%=============================================================================
function labels = defaultLabels(count)
  labels = cell(count, 1);
  for k = 1:count
    labels{k} = num2str(k);
  end
end
%=============================================================================
function tf = isBoxchartOptionName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  try
    canonicalPropertyName(value);
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
function name = canonicalPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:boxchart:propertyNameExpected', 'Property name expected.');
  end
  switch lower(char(value))
    case 'groupbycolor'
      name = 'GroupByColor';
    case 'xdata'
      name = 'XData';
    case 'ydata'
      name = 'YData';
    case 'xdatamode'
      name = 'XDataMode';
    case 'ydatamode'
      name = 'YDataMode';
    case 'xvariable'
      name = 'XVariable';
    case 'yvariable'
      name = 'YVariable';
    case 'boxwidth'
      name = 'BoxWidth';
    case 'boxfacecolor'
      name = 'BoxFaceColor';
    case 'boxedgecolor'
      name = 'BoxEdgeColor';
    case 'boxmedianlinecolor'
      name = 'BoxMedianLineColor';
    case 'boxfacealpha'
      name = 'BoxFaceAlpha';
    case 'capwidth'
      name = 'CapWidth';
    case 'whiskerlinecolor'
      name = 'WhiskerLineColor';
    case 'linewidth'
      name = 'LineWidth';
    case 'whiskerlinestyle'
      name = 'WhiskerLineStyle';
    case 'markercolor'
      name = 'MarkerColor';
    case 'markersize'
      name = 'MarkerSize';
    case {'markerstyle', 'marker'}
      name = 'MarkerStyle';
    case 'clipping'
      name = 'Clipping';
    case 'orientation'
      name = 'Orientation';
    case 'colorgrouplayout'
      name = 'ColorGroupLayout';
    case 'notch'
      name = 'Notch';
    case 'jitteroutliers'
      name = 'JitterOutliers';
    case 'colorgroupwidth'
      name = 'ColorGroupWidth';
    case 'boxfacecolormode'
      name = 'BoxFaceColorMode';
    case 'boxedgecolormode'
      name = 'BoxEdgeColorMode';
    case 'boxmedianlinecolormode'
      name = 'BoxMedianLineColorMode';
    case 'capwidthmode'
      name = 'CapWidthMode';
    case 'markercolormode'
      name = 'MarkerColorMode';
    case 'colorgroupwidthmode'
      name = 'ColorGroupWidthMode';
    case 'visible'
      name = 'Visible';
    case 'displayname'
      name = 'DisplayName';
    case 'tag'
      name = 'Tag';
    case 'userdata'
      name = 'UserData';
    case 'seriesindex'
      name = 'SeriesIndex';
    case 'handlevisibility'
      name = 'HandleVisibility';
    case 'hittest'
      name = 'HitTest';
    case 'pickableparts'
      name = 'PickableParts';
    case 'buttondownfcn'
      name = 'ButtonDownFcn';
    case 'contextmenu'
      name = 'ContextMenu';
    case 'busyaction'
      name = 'BusyAction';
    case 'interruptible'
      name = 'Interruptible';
    case 'createfcn'
      name = 'CreateFcn';
    case 'deletefcn'
      name = 'DeleteFcn';
    case 'selected'
      name = 'Selected';
    case 'selectionhighlight'
      name = 'SelectionHighlight';
    otherwise
      error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'boxchart');
  end
end
%=============================================================================
function validatePositiveScalar(value, name)
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) || value <= 0
    error('Nelson:graphics:mustBePositiveFinite', '%s must be a positive finite scalar.', name);
  end
end
%=============================================================================
function validateUnitScalar(value, name)
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) || value < 0 || value > 1
    error('Nelson:boxchart:mustBeInRange', '%s must be a scalar between 0 and 1.', name);
  end
end
%=============================================================================
function validateColorOrAuto(value, name)
  if isnumeric(value)
    if ~isequal(size(value), [1 3]) || any(value < 0) || any(value > 1)
      error('Nelson:boxchart:invalidRGBTriplet', '%s must be an RGB triplet.', name);
    end
  elseif ~(ischar(value) || isStringScalar(value))
    error('Nelson:boxchart:invalidColor', '%s must be a color value.', name);
  end
end
%=============================================================================
function validateMember(value, values, name)
  if ~(ischar(value) || isStringScalar(value)) || ~any(strcmpi(char(value), values))
    error('Nelson:boxchart:invalidValue', '%s has an unsupported value.', name);
  end
end
%=============================================================================
function validateOnOff(value, name)
  if islogical(value) && isscalar(value)
    return
  end
  if ~(ischar(value) || isStringScalar(value)) || ~any(strcmpi(char(value), {'on', 'off'}))
    error('Nelson:boxchart:mustBeOnOff', '%s must be ''on'' or ''off''.', name);
  end
end
%=============================================================================
