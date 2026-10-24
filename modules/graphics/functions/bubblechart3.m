%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bubblechart3(varargin)
  narginchk(4, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  if ~isempty(args) && (istable(args{1}) || istimetable(args{1}))
    [x, y, z, sizes, colorData, args, tableArgs] = parseTableBubbleData(args);
  else
    tableArgs = {};
    [x, y, z, sizes, colorData, args] = parseNumericBubbleData(args);
  end
  validateBubbleData(x, y, z, sizes);
  chartArgs = parseBubbleOptions(args);
  h = createBubbleChart3(parent, x, y, z, sizes, colorData, chartArgs, tableArgs);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [x, y, z, sizes, colorData, args] = parseNumericBubbleData(args)
  if numel(args) < 4 || ~isnumeric(args{1}) || ~isnumeric(args{2}) ...
      || ~isnumeric(args{3}) || ~isnumeric(args{4})
    error('Nelson:bubblechart3:numericXYExpected', 'Numeric X, Y, Z, and bubble size data expected.');
  end
  x = args{1};
  y = args{2};
  z = args{3};
  sizes = args{4};
  args = args(5:end);
  [colorData, args] = parseColorData(args);
end
%=============================================================================
function validateBubbleData(x, y, z, sizes)
  if isempty(sizes)
    error('Nelson:dimagree:sizeSMatchXYZ', 'Size of S must match X, Y, Z.');
  end
  if ~(isnumeric(x) && isnumeric(y) && isnumeric(z) && isnumeric(sizes))
    error('Nelson:bubblechart3:numericXYExpected', 'Numeric X, Y, Z, and bubble size data expected.');
  end
  if isvector(x) && isvector(y) && isvector(z)
    if numel(x) ~= numel(y) || numel(x) ~= numel(z)
      error('Nelson:dimagree:xYZSize', 'X, Y, and Z must be the same size.');
    end
    n = numel(x);
    if ~(isscalar(sizes) || numel(sizes) == n)
      error('Nelson:dimagree:sizeSMatchXYZ', 'Size of S must match X, Y, Z.');
    end
  elseif ~isequal(size(x), size(y)) || ~isequal(size(x), size(z))
    error('Nelson:dimagree:xYZSize', 'X, Y, and Z must be the same size.');
  elseif ~(isequal(size(sizes), size(x)) || numel(sizes) == numel(x))
    error('Nelson:dimagree:sizeSMatchXYZ', 'Size of S must match X, Y, Z.');
  end
end
%=============================================================================
function [colorData, args] = parseColorData(args)
  colorData = [];
  if isempty(args) || isBubblePropertyName(args{1})
    return
  end
  if isnumeric(args{1})
    colorData = args{1};
    args = args(2:end);
    return
  end
  if ischar(args{1}) || isStringScalar(args{1})
    try
      colorData = normalizeColorData(args{1});
      args = args(2:end);
    catch
      colorData = [];
    end
  end
end
%=============================================================================
function chartArgs = parseBubbleOptions(args)
  chartArgs = {};
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = char(args{k});
    value = args{k + 1};
    switch lower(name)
      case {'color', 'cdata'}
        chartArgs = [chartArgs, {'CData', normalizeColorData(value)}];
      case 'alphadata'
        chartArgs = [chartArgs, {'AlphaData', value}];
      case 'alphadatamapping'
        chartArgs = [chartArgs, {'AlphaDataMapping', value}];
      case 'alphadatamode'
        chartArgs = [chartArgs, {'AlphaDataMode', value}];
      case 'cdatamode'
        chartArgs = [chartArgs, {'CDataMode', value}];
      case 'linewidth'
        chartArgs = [chartArgs, {'LineWidth', value}];
      case 'markerfacecolor'
        chartArgs = [chartArgs, {'MarkerFaceColor', value}];
      case 'markeredgecolor'
        chartArgs = [chartArgs, {'MarkerEdgeColor', value}];
      case 'markerfacealpha'
        chartArgs = [chartArgs, {'MarkerFaceAlpha', value}];
      case 'markeredgealpha'
        chartArgs = [chartArgs, {'MarkerEdgeAlpha', value}];
      case 'displayname'
        chartArgs = [chartArgs, {'DisplayName', value}];
      case 'visible'
        chartArgs = [chartArgs, {'Visible', value}];
      case 'tag'
        chartArgs = [chartArgs, {'Tag', value}];
      case 'userdata'
        chartArgs = [chartArgs, {'UserData', value}];
      case 'sourcetable'
        chartArgs = [chartArgs, {'SourceTable', value}];
      case 'xvariable'
        chartArgs = [chartArgs, {'XVariable', value}];
      case 'yvariable'
        chartArgs = [chartArgs, {'YVariable', value}];
      case 'zvariable'
        chartArgs = [chartArgs, {'ZVariable', value}];
      case 'sizevariable'
        chartArgs = [chartArgs, {'SizeVariable', value}];
      case 'colorvariable'
        chartArgs = [chartArgs, {'ColorVariable', value}];
      case 'xdatasource'
        chartArgs = [chartArgs, {'XDataSource', value}];
      case 'ydatasource'
        chartArgs = [chartArgs, {'YDataSource', value}];
      case 'zdatasource'
        chartArgs = [chartArgs, {'ZDataSource', value}];
      case 'sizedatasource'
        chartArgs = [chartArgs, {'SizeDataSource', value}];
      case 'cdatasource'
        chartArgs = [chartArgs, {'CDataSource', value}];
      case 'sizedatamode'
        chartArgs = [chartArgs, {'SizeDataMode', value}];
      case 'xdatamode'
        chartArgs = [chartArgs, {'XDataMode', value}];
      case 'ydatamode'
        chartArgs = [chartArgs, {'YDataMode', value}];
      case 'zdatamode'
        chartArgs = [chartArgs, {'ZDataMode', value}];
      case 'seriesindex'
        chartArgs = [chartArgs, {'SeriesIndex', value}];
      case 'handlevisibility'
        chartArgs = [chartArgs, {'HandleVisibility', value}];
      case 'clipping'
        chartArgs = [chartArgs, {'Clipping', value}];
      case 'buttondownfcn'
        chartArgs = [chartArgs, {'ButtonDownFcn', value}];
      case 'createfcn'
        chartArgs = [chartArgs, {'CreateFcn', value}];
      case 'deletefcn'
        chartArgs = [chartArgs, {'DeleteFcn', value}];
      case 'busyaction'
        chartArgs = [chartArgs, {'BusyAction', value}];
      case 'interruptible'
        chartArgs = [chartArgs, {'Interruptible', value}];
      case 'pickableparts'
        chartArgs = [chartArgs, {'PickableParts', value}];
      case 'hittest'
        chartArgs = [chartArgs, {'HitTest', value}];
      case 'selected'
        chartArgs = [chartArgs, {'Selected', value}];
      case 'selectionhighlight'
        chartArgs = [chartArgs, {'SelectionHighlight', value}];
      case 'contextmenu'
        chartArgs = [chartArgs, {'ContextMenu', value}];
      case 'datatiptemplate'
        chartArgs = [chartArgs, {'DataTipTemplate', value}];
      case 'annotation'
        chartArgs = [chartArgs, {'Annotation', value}];
      case 'alphavariable'
        chartArgs = [chartArgs, {'AlphaVariable', value}];
      case 'xjitter'
        chartArgs = [chartArgs, {'XJitter', value}];
      case 'yjitter'
        chartArgs = [chartArgs, {'YJitter', value}];
      case 'zjitter'
        chartArgs = [chartArgs, {'ZJitter', value}];
      case 'xjitterwidth'
        chartArgs = [chartArgs, {'XJitterWidth', value}];
      case 'xjitterwidthmode'
        chartArgs = [chartArgs, {'XJitterWidthMode', value}];
      case 'yjitterwidth'
        chartArgs = [chartArgs, {'YJitterWidth', value}];
      case 'yjitterwidthmode'
        chartArgs = [chartArgs, {'YJitterWidthMode', value}];
      case 'zjitterwidth'
        chartArgs = [chartArgs, {'ZJitterWidth', value}];
      case 'zjitterwidthmode'
        chartArgs = [chartArgs, {'ZJitterWidthMode', value}];
      case 'xjitterdirection'
        chartArgs = [chartArgs, {'XJitterDirection', value}];
      case 'yjitterdirection'
        chartArgs = [chartArgs, {'YJitterDirection', value}];
      case 'zjitterdirection'
        chartArgs = [chartArgs, {'ZJitterDirection', value}];
      otherwise
        error('Nelson:bubblechart3:unknownProperty', ['Unrecognized property ', name, ' for class BubbleChart.']);
    end
  end
end
%=============================================================================
function h = createBubbleChart3(parent, x, y, z, sizes, colorData, chartArgs, tableArgs)
  parent = prepareParent(parent);
  [orientation, nSeries] = getSeriesOrientation3(x, y, z, sizes);
  h = [];
  for k = 1:nSeries
    xk = graphicsSelectSeriesData(x, orientation, k, 'row');
    yk = graphicsSelectSeriesData(y, orientation, k, 'row');
    zk = graphicsSelectSeriesData(z, orientation, k, 'row');
    sk = graphicsSelectSeriesData(sizes, orientation, k, 'row');
    ck = graphicsSelectColorSeriesData(colorData, orientation, k, nSeries, numel(xk), 'row');
    args = [chartArgs, graphicsSelectTableSeriesArgs(tableArgs, k, nSeries)];
    if isempty(ck) && ~hasConstructorProperty(args, 'CData')
      ck = getColorAndUpdateIndex(parent);
    end
    h = [h, createBubbleChart3Object(parent, xk, yk, zk, sk, ck, args)];
  end
  view(parent, 3);
  grid(parent, 'on');
  parent.Box = 'on';
  updateBubbleChart3Axes(parent, x, y, z);
  applyBubbleAxisLabels3(parent, tableArgs);
end
%=============================================================================
function applyBubbleAxisLabels3(ax, tableArgs)
  if isempty(ax) || ~isscalar(ax) || ~isgraphics(ax, 'axes') || isempty(tableArgs)
    return
  end
  names = tableArgs{1};
  xName = getNamedArgValue(names, 'XVariable');
  yName = getNamedArgValue(names, 'YVariable');
  zName = getNamedArgValue(names, 'ZVariable');
  if ~isempty(xName) && isAxisLabelEmpty(ax.XLabel)
    xlabel(ax, xName);
  end
  if ~isempty(yName) && isAxisLabelEmpty(ax.YLabel)
    ylabel(ax, yName);
  end
  if ~isempty(zName) && isAxisLabelEmpty(ax.ZLabel)
    zlabel(ax, zName);
  end
end
%=============================================================================
function value = getNamedArgValue(args, name)
  value = '';
  for k = 1:2:numel(args)
    if ischar(args{k}) && strcmpi(args{k}, name)
      value = args{k + 1};
      return
    end
  end
end
%=============================================================================
function tf = isAxisLabelEmpty(labelHandle)
  tf = true;
  if ~isempty(labelHandle) && isscalar(labelHandle) && isgraphics(labelHandle)
    tf = isempty(char(labelHandle.String));
  end
end
%=============================================================================
function [orientation, nSeries] = getSeriesOrientation3(x, y, z, sizes)
  if isvector(x) && isvector(y) && isvector(z) && isscalar(sizes)
    orientation = 'points';
    nSeries = numel(x);
    return
  end
  if isvector(x) && isvector(y) && isvector(z)
    orientation = 'single';
    nSeries = 1;
    return
  end
  if isvector(sizes) && numel(sizes) == numel(x)
    orientation = 'single';
    nSeries = 1;
    return
  end
  rows = size(x, 1);
  cols = size(x, 2);
  if isequal(size(sizes), size(x)) || (isvector(sizes) && numel(sizes) == rows)
    orientation = 'columns';
    nSeries = cols;
  elseif isrow(sizes) && numel(sizes) == cols && rows > 1
    orientation = 'rows';
    nSeries = rows;
  else
    error('Nelson:dimagree:sizeSMatchXYZ', 'Size of S must match X, Y, Z.');
  end
end
%=============================================================================
function h = createBubbleChart3Object(parent, x, y, z, sizes, colorData, chartArgs)
  args = {'Parent', parent, 'XData', x, 'YData', y, 'ZData', z, 'SizeData', sizes};
  if isempty(colorData) && ~hasConstructorProperty(chartArgs, 'CData')
    colorData = getColorAndUpdateIndex(parent);
  end
  if ~isempty(colorData)
    args = [args, {'CData', colorData}];
  end
  h = __bubblechart__(args{:}, chartArgs{:});
end
%=============================================================================
function updateBubbleChart3Axes(parent, x, y, z)
  xlim = paddedDataLimits(x, 0.15063157016657);
  ylim = paddedDataLimits(y, 0.19772113500799);
  zlim = paddedDataLimits(z, 0.22645739910314);
  if ~isempty(xlim)
    parent.XLim = xlim;
    parent.XTick = linearTicksWithin(xlim);
  end
  if ~isempty(ylim)
    parent.YLim = ylim;
    parent.YTick = linearTicksWithin(ylim);
  end
  if ~isempty(zlim)
    parent.ZLim = zlim;
    parent.ZTick = linearTicksWithin(zlim);
  end
end
%=============================================================================
function limits = paddedDataLimits(values, ratio)
  values = double(values(:));
  values = values(isfinite(values));
  if isempty(values)
    limits = [];
    return
  end
  vmin = min(values);
  vmax = max(values);
  span = vmax - vmin;
  if span == 0
    pad = 0.5;
  else
    pad = span * ratio;
  end
  limits = [vmin - pad, vmax + pad];
end
%=============================================================================
function ticks = linearTicksWithin(limits)
  span = limits(2) - limits(1);
  if ~isfinite(span) || span <= 0
    ticks = [];
    return
  end
  step = niceTickStep(span / 6);
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  ticks = first:step:last;
  ticks(abs(ticks) < step * 1e-10) = 0;
end
%=============================================================================
function step = niceTickStep(rawStep)
  base = 10 ^ floor(log10(rawStep));
  fraction = rawStep / base;
  if fraction <= 1.5
    multiplier = 1;
  elseif fraction <= 3
    multiplier = 2;
  elseif fraction <= 7
    multiplier = 5;
  else
    multiplier = 10;
  end
  step = multiplier * base;
end
%=============================================================================
function parent = prepareParent(parent)
  if isempty(parent)
    parent = newplot();
  else
    parent = newplot(parent);
  end
end
%=============================================================================
function value = normalizeColorData(value)
  if ischar(value) || isStringScalar(value)
    value = validatecolor(value);
  end
end
%=============================================================================
function tf = isBubblePropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  names = {'AlphaData', 'AlphaDataMapping', 'AlphaDataMode', 'Annotation', ...
    'AlphaVariable', 'BusyAction', 'ButtonDownFcn', 'CData', 'CDataMode', ...
    'CDataSource', 'Clipping', 'Color', 'ColorVariable', 'ContextMenu', ...
    'CreateFcn', 'DataTipTemplate', 'DeleteFcn', 'DisplayName', ...
    'HandleVisibility', 'HitTest', 'Interruptible', 'LineWidth', ...
    'MarkerEdgeAlpha', 'MarkerEdgeColor', 'MarkerFaceAlpha', ...
    'MarkerFaceColor', 'PickableParts', 'Selected', 'SelectionHighlight', ...
    'SeriesIndex', 'SizeDataMode', 'SizeDataSource', 'SizeVariable', ...
    'SourceTable', 'Tag', 'UserData', 'Visible', 'XDataMode', 'XDataSource', ...
    'XJitter', 'XJitterDirection', 'XJitterWidth', 'XVariable', ...
    'YDataMode', 'YDataSource', 'YJitter', 'YJitterDirection', ...
    'YJitterWidth', 'YVariable', 'ZDataMode', 'ZDataSource', ...
    'ZJitter', 'ZJitterDirection', 'ZJitterWidth', 'ZVariable'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
function [x, y, z, sizes, colorData, args, tableArgs] = parseTableBubbleData(args)
  tbl = args{1};
  if numel(args) < 5
    error('Nelson:graphics:tableVariableExpected', 'Table variable arguments expected.');
  end
  xNames = graphicsTableVariableNames(tbl, args{2});
  yNames = graphicsTableVariableNames(tbl, args{3});
  zNames = graphicsTableVariableNames(tbl, args{4});
  sizeNames = graphicsTableVariableNames(tbl, args{5});
  args = args(6:end);
  colorNames = {};
  if ~isempty(args) && ~isBubblePropertyName(args{1})
    colorNames = graphicsTableVariableNames(tbl, args{1});
    args = args(2:end);
  end
  nSeries = max([numel(xNames), numel(yNames), numel(zNames), numel(sizeNames), max(1, numel(colorNames))]);
  validateTableVariableCounts(xNames, yNames, zNames, sizeNames, colorNames, nSeries);
  [x, y, z, sizes, colorData, tableArgs] ...
    = tableBubbleSeries(tbl, xNames, yNames, zNames, sizeNames, colorNames, nSeries);
end
%=============================================================================
function validateTableVariableCounts(xNames, yNames, zNames, sizeNames, colorNames, nSeries)
  counts = [numel(xNames), numel(yNames), numel(zNames), numel(sizeNames)];
  if ~isempty(colorNames)
    counts(end + 1) = numel(colorNames);
  end
  if any(counts ~= 1 & counts ~= nSeries)
    error('Nelson:dimagree:tableVariableCountsMatch', 'Table variable counts must match.');
  end
end
%=============================================================================
function [x, y, z, sizes, colorData, tableArgs] = tableBubbleSeries(tbl, xNames, yNames, zNames, sizeNames, colorNames, nSeries)
  x = [];
  y = [];
  z = [];
  sizes = [];
  colorData = [];
  tableArgs = cell(1, nSeries);
  for k = 1:nSeries
    xName = graphicsSelectName(xNames, k);
    yName = graphicsSelectName(yNames, k);
    zName = graphicsSelectName(zNames, k);
    sizeName = graphicsSelectName(sizeNames, k);
    x = graphicsAppendColumn(x, graphicsNumericColumn(tbl.(xName), xName));
    y = graphicsAppendColumn(y, graphicsNumericColumn(tbl.(yName), yName));
    z = graphicsAppendColumn(z, graphicsNumericColumn(tbl.(zName), zName));
    sizes = graphicsAppendColumn(sizes, graphicsNumericColumn(tbl.(sizeName), sizeName));
    oneArgs = {'SourceTable', tbl, 'XVariable', xName, 'YVariable', yName, ...
      'ZVariable', zName, 'SizeVariable', sizeName};
    if ~isempty(colorNames)
      colorName = graphicsSelectName(colorNames, k);
      colorData = graphicsAppendColumn(colorData, graphicsNumericColumn(tbl.(colorName), colorName));
      oneArgs(end + 1:end + 2) = {'ColorVariable', colorName};
    end
    if nSeries > 1
      oneArgs(end + 1:end + 2) = {'DisplayName', yName};
    end
    tableArgs{k} = oneArgs;
  end
end
%=============================================================================
function tf = hasConstructorProperty(args, propertyName)
  tf = false;
  for k = 1:2:numel(args)
    if ischar(args{k}) && strcmpi(args{k}, propertyName)
      tf = true;
      return
    end
  end
end
%=============================================================================
