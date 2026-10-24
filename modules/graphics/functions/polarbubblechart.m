%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = polarbubblechart(varargin)
  narginchk(3, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  if ~isempty(args) && ~(istable(args{1}) || istimetable(args{1})) ...
      && isscalar(args{1}) && isgraphics(args{1}) ...
      && isgraphics(args{1}, 'axes') && ~isgraphics(args{1}, 'polaraxes')
    error('Nelson:graphics:invalidParentInput', 'Parent input must be a polaraxes object.');
  end
  [ax, args] = polarParseTargetAxes(args);
  ax = ensurePolarAxes(ax);
  if ~isempty(args) && (istable(args{1}) || istimetable(args{1}))
    [theta, radius, bubbleSize, colorData, args, tableArgs] = parseTableBubbleData(args);
  else
    tableArgs = {};
    [theta, radius, bubbleSize, colorData, args] = parseNumericBubbleData(args);
  end
  [chartArgs, rendererArgs] = parseBubbleOptions(args);
  [orientation, nSeries] = getSeriesOrientation(theta, radius, bubbleSize);
  [fig, previousDrawLater] = polarBeginDrawLater(ax);
  try
    ax = polarPrepareDataAxes(ax);
    h = createPolarBubbleSeries(ax, theta, radius, bubbleSize, colorData, orientation, nSeries, chartArgs, tableArgs);
    polarAppendDataHandles(ax, h);
    polarEndDrawLater(fig, previousDrawLater);
  catch exception
    polarEndDrawLater(fig, previousDrawLater);
    error(exception.message);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function h = createPolarBubbleSeries(ax, theta, radius, bubbleSize, colorData, orientation, nSeries, chartArgs, tableArgs)
  if nSeries == 1
    tk = graphicsSelectSeriesData(theta, orientation, 1, 'row');
    rk = graphicsSelectSeriesData(radius, orientation, 1, 'row');
    sk = graphicsSelectSeriesData(bubbleSize, orientation, 1, 'row');
    ck = graphicsSelectColorSeriesData(colorData, orientation, 1, nSeries, numel(tk), 'row');
    args = [chartArgs, graphicsSelectTableSeriesArgs(tableArgs, 1, nSeries)];
    h = createPolarBubbleObject(ax, tk, rk, sk, ck, args);
    state = struct();
    state.ChartType = 'polarbubblechart';
    state.BubbleSizeData = sk;
    state.Renderer = h;
    h.UserData = state;
    return
  end
  handles = cell(1, nSeries);
  for k = 1:nSeries
    tk = graphicsSelectSeriesData(theta, orientation, k, 'row');
    rk = graphicsSelectSeriesData(radius, orientation, k, 'row');
    sk = graphicsSelectSeriesData(bubbleSize, orientation, k, 'row');
    ck = graphicsSelectColorSeriesData(colorData, orientation, k, nSeries, numel(tk), 'row');
    args = [chartArgs, graphicsSelectTableSeriesArgs(tableArgs, k, nSeries)];
    hk = createPolarBubbleObject(ax, tk, rk, sk, ck, args);
    state = struct();
    state.ChartType = 'polarbubblechart';
    state.BubbleSizeData = sk;
    state.Renderer = hk;
    hk.UserData = state;
    handles{k} = hk;
  end
  h = [handles{:}];
end
%=============================================================================
function ax = ensurePolarAxes(ax)
  if isgraphics(ax, 'polaraxes')
    return
  end
  ax = polarInitializeAxes(ax, false);
end
%=============================================================================
function [theta, radius, bubbleSize, colorData, args] = parseNumericBubbleData(args)
  if numel(args) < 3 || ~isnumeric(args{1}) || ~isnumeric(args{2}) || ~isnumeric(args{3})
    error('Nelson:polarbubblechart:numericThetaRadiusExpected', 'Numeric theta, radius, and bubble size data expected.');
  end
  theta = args{1};
  radius = args{2};
  bubbleSize = args{3};
  args = args(4:end);
  colorData = [];
  if ~isempty(args) && (isnumeric(args{1}) || isColorText(args{1}))
    colorData = normalizeColorData(args{1});
    args = args(2:end);
  end
end
%=============================================================================
function h = createPolarBubbleObject(ax, theta, radius, bubbleSize, colorData, chartArgs)
  if ~isempty(colorData)
    chartArgs = [chartArgs, {'CData', colorData}];
  end
  h = __bubblechart__('Parent', ax, 'XData', [], 'YData', [], ...
    'ThetaData', theta, 'RData', radius, 'SizeData', bubbleSize, chartArgs{:});
end
%=============================================================================
function [chartArgs, rendererArgs] = parseBubbleOptions(args)
  chartArgs = {};
  rendererArgs = {};
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = char(args{k});
    value = args{k + 1};
    switch lower(name)
      case 'markeredgecolor'
        chartArgs = [chartArgs, {'MarkerEdgeColor', value}];
        rendererArgs = [rendererArgs, {'MarkerEdgeColor', value}];
      case 'markerfacecolor'
        chartArgs = [chartArgs, {'MarkerFaceColor', value}];
        rendererArgs = [rendererArgs, {'MarkerFaceColor', value}];
      case 'markeredgealpha'
        chartArgs = [chartArgs, {'MarkerEdgeAlpha', value}];
      case 'markerfacealpha'
        chartArgs = [chartArgs, {'MarkerFaceAlpha', value}];
      case 'linewidth'
        chartArgs = [chartArgs, {'LineWidth', value}];
        rendererArgs = [rendererArgs, {'LineWidth', value}];
      case {'displayname', 'visible', 'tag', 'userdata', 'handlevisibility'}
        chartArgs = [chartArgs, {name, value}];
      case 'sourcetable'
        chartArgs = [chartArgs, {'SourceTable', value}];
      case 'thetavariable'
        chartArgs = [chartArgs, {'ThetaVariable', value}];
      case 'rvariable'
        chartArgs = [chartArgs, {'RVariable', value}];
      case 'sizevariable'
        chartArgs = [chartArgs, {'SizeVariable', value}];
      case 'colorvariable'
        chartArgs = [chartArgs, {'ColorVariable', value}];
      otherwise
        error('Nelson:polarbubblechart:unknownProperty', ['Unrecognized property ', name, ' for class BubbleChart.']);
    end
  end
end
%=============================================================================
function tf = isColorText(value)
  tf = (ischar(value) || isStringScalar(value)) ...
    && (any(strcmpi(char(value), getColorShortNameList())) || any(strcmpi(char(value), getColorNameList())));
end
%=============================================================================
function colorData = normalizeColorData(value)
  if ischar(value) || isStringScalar(value)
    colorData = validatecolor(value);
  else
    colorData = value;
  end
end
%=============================================================================
function [theta, radius, bubbleSize, colorData, args, tableArgs] = parseTableBubbleData(args)
  tbl = args{1};
  if numel(args) < 4
    error('Nelson:graphics:tableVariableExpected', 'Table variable arguments expected.');
  end
  thetaNames = graphicsTableVariableNames(tbl, args{2});
  radiusNames = graphicsTableVariableNames(tbl, args{3});
  sizeNames = graphicsTableVariableNames(tbl, args{4});
  args = args(5:end);
  colorNames = {};
  if ~isempty(args) && ~isPolarBubblePropertyName(args{1})
    colorNames = graphicsTableVariableNames(tbl, args{1});
    args = args(2:end);
  end
  nSeries = max([numel(thetaNames), numel(radiusNames), numel(sizeNames), max(1, numel(colorNames))]);
  validateTableVariableCounts(thetaNames, radiusNames, sizeNames, colorNames, nSeries);
  [theta, radius, bubbleSize, colorData, tableArgs] ...
    = tableBubbleSeries(tbl, thetaNames, radiusNames, sizeNames, colorNames, nSeries);
end
%=============================================================================
function validateTableVariableCounts(thetaNames, radiusNames, sizeNames, colorNames, nSeries)
  counts = [numel(thetaNames), numel(radiusNames), numel(sizeNames)];
  if ~isempty(colorNames)
    counts(end + 1) = numel(colorNames);
  end
  if any(counts ~= 1 & counts ~= nSeries)
    error('Nelson:dimagree:tableVariableCountsMatch', 'Table variable counts must match.');
  end
end
%=============================================================================
function [theta, radius, bubbleSize, colorData, tableArgs] ...
    = tableBubbleSeries(tbl, thetaNames, radiusNames, sizeNames, colorNames, nSeries)
  theta = [];
  radius = [];
  bubbleSize = [];
  colorData = [];
  tableArgs = cell(1, nSeries);
  for k = 1:nSeries
    thetaName = graphicsSelectName(thetaNames, k);
    radiusName = graphicsSelectName(radiusNames, k);
    sizeName = graphicsSelectName(sizeNames, k);
    theta = graphicsAppendColumn(theta, graphicsNumericColumn(tbl.(thetaName), thetaName));
    radius = graphicsAppendColumn(radius, graphicsNumericColumn(tbl.(radiusName), radiusName));
    bubbleSize = graphicsAppendColumn(bubbleSize, graphicsNumericColumn(tbl.(sizeName), sizeName));
    oneArgs = {'SourceTable', tbl, 'ThetaVariable', thetaName, 'RVariable', radiusName, ...
      'SizeVariable', sizeName};
    if ~isempty(colorNames)
      colorName = graphicsSelectName(colorNames, k);
      colorData = graphicsAppendColumn(colorData, graphicsNumericColumn(tbl.(colorName), colorName));
      oneArgs(end + 1:end + 2) = {'ColorVariable', colorName};
    end
    if nSeries > 1
      oneArgs(end + 1:end + 2) = {'DisplayName', radiusName};
    end
    tableArgs{k} = oneArgs;
  end
end
%=============================================================================
function [orientation, nSeries] = getSeriesOrientation(theta, radius, bubbleSize)
  if isvector(theta) && isvector(radius) && isscalar(bubbleSize)
    orientation = 'points';
    nSeries = numel(theta);
    return
  end
  if isvector(theta) && isvector(radius)
    orientation = 'single';
    nSeries = 1;
    return
  end
  if isvector(theta) && ~isvector(radius)
    rows = size(radius, 1);
    cols = size(radius, 2);
    if numel(theta) == rows && (isscalar(bubbleSize) || (isvector(bubbleSize) && numel(bubbleSize) == rows))
      orientation = 'columns';
      nSeries = cols;
      return
    end
  end
  if ~isvector(theta) && isvector(radius)
    rows = size(theta, 1);
    cols = size(theta, 2);
    if numel(radius) == rows && (isscalar(bubbleSize) || (isvector(bubbleSize) && numel(bubbleSize) == rows))
      orientation = 'columns';
      nSeries = cols;
      return
    end
  end
  if isvector(bubbleSize) && numel(bubbleSize) == numel(theta) && isvector(radius)
    orientation = 'single';
    nSeries = 1;
    return
  end
  rows = size(theta, 1);
  cols = size(theta, 2);
  if isequal(size(bubbleSize), size(theta)) || (isvector(bubbleSize) && numel(bubbleSize) == rows)
    orientation = 'columns';
    nSeries = cols;
  elseif isrow(bubbleSize) && numel(bubbleSize) == cols && rows > 1
    orientation = 'rows';
    nSeries = rows;
  else
    error('Nelson:polarbubblechart:invalidSizeData', 'Size data must match theta and radius data.');
  end
end
%=============================================================================
function tf = isPolarBubblePropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  names = {'ColorVariable', 'DisplayName', 'HandleVisibility', 'MarkerEdgeColor', ...
    'MarkerFaceColor', 'MarkerEdgeAlpha', 'MarkerFaceAlpha', 'LineWidth', ...
    'RVariable', 'SizeVariable', 'SourceTable', 'Tag', 'ThetaVariable', ...
    'UserData', 'Visible'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
