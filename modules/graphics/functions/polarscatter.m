%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = polarscatter(varargin)
  narginchk(2, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  if ~isempty(args) && ~(istable(args{1}) || istimetable(args{1})) ...
      && isscalar(args{1}) && isgraphics(args{1}, 'axes') && ~isgraphics(args{1}, 'polaraxes')
    error('Nelson:graphics:invalidParentInput', 'Parent input must be a polaraxes object.');
  end
  [ax, args] = polarParseTargetAxes(args);
  if ~isempty(args) && (istable(args{1}) || istimetable(args{1}))
    [theta, radius, args, tableArgs] = parseTableScatterData(args);
  else
    tableArgs = {};
    [theta, radius, args] = parseNumericScatterData(args);
  end
  [opts, chartProps, rendererProps] = parsePolarScatterOptions(args);
  [orientation, nSeries] = getSeriesOrientation(theta, radius);
  if nSeries > 1
    h = createPolarScatterSeries(ax, theta, radius, orientation, nSeries, opts, chartProps, tableArgs);
    if nargout == 1
      varargout{1} = h;
    end
    return
  end
  theta = graphicsSelectSeriesData(theta, orientation, 1, 'column');
  radius = graphicsSelectSeriesData(radius, orientation, 1, 'column');
  chartProps = [chartProps, graphicsSelectTableSeriesArgs(tableArgs, 1, nSeries)];
  [fig, previousDrawLater] = polarBeginDrawLater(ax);
  try
    ax = polarPrepareDataAxes(ax);
    if isempty(opts.Color)
      opts.Color = getColorAndUpdateIndex(ax);
    end
    renderer = createPolarMarkerLine(ax, theta, radius, opts, rendererProps);
    h = createPolarScatterObject(ax, theta, radius, opts, chartProps);
    state = h.UserData;
    state.ChartType = 'polarscatter';
    state.Renderer = renderer;
    h.UserData = state;
    polarAppendDataHandles(ax, renderer);
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
function [theta, radius, args] = parseNumericScatterData(args)
  if numel(args) < 2 || ~isnumeric(args{1}) || ~isnumeric(args{2})
    error('Nelson:polarscatter:numericThetaAndExpected', 'Numeric theta and radius data expected.');
  end
  theta = double(args{1});
  radius = double(args{2});
  if numel(theta) ~= numel(radius)
    error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'Theta', 'radius');
  end
  theta = theta(:);
  radius = radius(:);
  args = args(3:end);
end
%=============================================================================
function [opts, chartProps, rendererProps] = parsePolarScatterOptions(args)
  opts = struct('SizeData', 36, 'Color', [], 'Marker', 'o', 'Filled', false);
  if ~isempty(args) && isnumeric(args{1})
    opts.SizeData = args{1};
    args = args(2:end);
  end
  if ~isempty(args) && (isnumeric(args{1}) || isColorText(args{1}))
    opts.Color = args{1};
    args = args(2:end);
  end
  [args, opts.Filled] = extractFilled(args);
  [args, markerFound, marker] = extractMarker(args);
  if markerFound
    opts.Marker = marker;
  end
  [chartProps, rendererProps] = parseNameValues(args);
end
%=============================================================================
function h = createPolarScatterSeries(ax, theta, radius, orientation, nSeries, opts, chartProps, tableArgs)
  handles = cell(1, nSeries);
  for k = 1:nSeries
    tk = graphicsSelectSeriesData(theta, orientation, k, 'column');
    rk = graphicsSelectSeriesData(radius, orientation, k, 'column');
    args = [chartProps, graphicsSelectTableSeriesArgs(tableArgs, k, nSeries)];
    handles{k} = createPolarScatterObject(ax, tk, rk, opts, args);
  end
  h = [handles{:}];
end
%=============================================================================
function h = createPolarMarkerLine(ax, theta, radius, opts, lineProps)
  color = opts.Color;
  if ischar(color) || isStringScalar(color)
    color = validatecolor(color);
  end
  % Per-point colour data (a vector of scalars, or an Nx3 matrix) maps through
  % the colormap to one colour per marker, exactly like a numeric C in scatter.
  color = resolvePolarPointColors(ax, color, numel(theta));
  markerSizes = markerSizeFromSizeData(opts.SizeData, numel(theta));
  perPointColor = isnumeric(color) && ismatrix(color) && size(color, 2) == 3 ...
    && size(color, 1) == numel(theta) && numel(theta) > 1;
  if isscalar(markerSizes) && ~perPointColor
    properties = {'LineStyle', 'none', 'Marker', opts.Marker, ...
      'MarkerSize', markerSizes, 'Color', color, 'MarkerEdgeColor', color};
    if opts.Filled
      properties = [properties, {'MarkerFaceColor', color, 'MarkerEdgeColor', 'none'}];
    end
    h = __line__('Parent', ax, 'XData', [], 'YData', [], ...
      'ThetaData', theta, 'RData', radius, properties{:}, lineProps{:});
  else
    h = [];
    theta = theta(:);
    radius = radius(:);
    if isscalar(markerSizes)
      markerSizes = repmat(markerSizes, 1, numel(theta));
    end
    for k = 1:numel(theta)
      pointColor = colorAtPoint(color, k);
      properties = {'LineStyle', 'none', 'Marker', opts.Marker, ...
        'MarkerSize', markerSizes(k), 'Color', pointColor, 'MarkerEdgeColor', pointColor};
      if opts.Filled
        properties = [properties, {'MarkerFaceColor', pointColor, 'MarkerEdgeColor', 'none'}];
      end
      h = [h, __line__('Parent', ax, 'XData', [], 'YData', [], ...
        'ThetaData', theta(k), 'RData', radius(k), properties{:}, lineProps{:})];
    end
  end
  state = struct('ChartType', 'polarscatter', 'SizeData', opts.SizeData);
  for k = 1:numel(h)
    h(k).UserData = state;
  end
end
%=============================================================================
function h = createPolarScatterObject(ax, theta, radius, opts, chartProps)
  color = opts.Color;
  if ischar(color) || isStringScalar(color)
    color = validatecolor(color);
  end
  properties = {'Parent', ax, 'XData', [], 'YData', [], ...
    'ThetaData', theta, 'RData', radius, 'SizeData', opts.SizeData, ...
    'CData', color, 'Marker', opts.Marker};
  if opts.Filled
    properties = [properties, {'MarkerFaceColor', 'flat', 'MarkerEdgeColor', 'none'}];
  end
  h = __scatter__(properties{:}, chartProps{:});
  state = struct('ChartType', 'polarscatter', 'SizeData', opts.SizeData);
  h.UserData = state;
end
%=============================================================================
function markerSize = markerSizeFromSizeData(sizeData, n)
  if isempty(sizeData)
    sizeData = 36;
  end
  if ~isnumeric(sizeData) || any(sizeData(:) <= 0)
    error('Nelson:validators:mustBePositive', '%s must be positive.', 'Size data');
  end
  markerSize = sqrt(double(sizeData(:).'));
  if isscalar(markerSize)
    return
  end
  if numel(markerSize) ~= n
    markerSize = markerSize(1);
  end
end
%=============================================================================
function rgb = resolvePolarPointColors(ax, color, n)
  rgb = color;
  if isempty(color) || ischar(color) || isStringScalar(color) || ~isnumeric(color)
    return
  end
  if ismatrix(color) && size(color, 2) == 3 && size(color, 1) == n
    return
  end
  if isvector(color) && numel(color) == n && n > 1
    cd = double(color(:));
    cmap = polarColormap(ax);
    lo = min(cd);
    hi = max(cd);
    if ~(hi > lo)
      hi = lo + 1;
    end
    m = size(cmap, 1);
    idx = round((cd - lo) / (hi - lo) * (m - 1)) + 1;
    idx = min(max(idx, 1), m);
    rgb = cmap(idx, :);
  end
end
function cmap = polarColormap(ax)
  cmap = parula(256);
  fig = ancestor(ax, 'figure');
  if ~isempty(fig)
    try
      cmap = get(fig, 'Colormap');
    catch
    end
  end
  if isempty(cmap) || size(cmap, 2) ~= 3
    cmap = parula(256);
  end
end
function color = colorAtPoint(colorData, k)
  color = colorData;
  if isnumeric(colorData) && ismatrix(colorData) && size(colorData, 2) == 3 ...
      && size(colorData, 1) >= k
    color = colorData(k, :);
  end
end
%=============================================================================
function tf = isColorText(value)
  tf = (ischar(value) || isStringScalar(value)) ...
    && (any(strcmpi(char(value), getColorShortNameList())) || any(strcmpi(char(value), getColorNameList())));
end
%=============================================================================
function [args, found] = extractFilled(args)
  found = false;
  for k = numel(args):-1:1
    if (ischar(args{k}) || isStringScalar(args{k})) && strcmpi(char(args{k}), 'filled')
      found = true;
      args(k) = [];
    end
  end
end
%=============================================================================
function [args, found, marker] = extractMarker(args)
  found = false;
  marker = 'o';
  markers = getMarkerNameList();
  for k = numel(args):-1:1
    if (ischar(args{k}) || isStringScalar(args{k})) && any(strcmpi(char(args{k}), markers))
      found = true;
      marker = char(args{k});
      args(k) = [];
      if k > 1 && (ischar(args{k - 1}) || isStringScalar(args{k - 1})) && strcmpi(char(args{k - 1}), 'Marker')
        args(k - 1) = [];
      end
      return
    end
  end
end
%=============================================================================
function [chartProps, rendererProps] = parseNameValues(args)
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  chartProps = {};
  rendererProps = {};
  for k = 1:2:length(args)
    name = char(args{k});
    value = args{k + 1};
    switch lower(name)
      case 'marker'
        chartProps = [chartProps, {'Marker', char(value)}];
        rendererProps = [rendererProps, {'Marker', char(value)}];
      case 'markeredgecolor'
        chartProps = [chartProps, {'MarkerEdgeColor', value}];
        rendererProps = [rendererProps, {'MarkerEdgeColor', value}];
      case 'markerfacecolor'
        chartProps = [chartProps, {'MarkerFaceColor', value}];
        rendererProps = [rendererProps, {'MarkerFaceColor', value}];
      case 'markeredgealpha'
        chartProps = [chartProps, {'MarkerEdgeAlpha', value}];
      case 'markerfacealpha'
        chartProps = [chartProps, {'MarkerFaceAlpha', value}];
      case 'linewidth'
        chartProps = [chartProps, {'LineWidth', value}];
        rendererProps = [rendererProps, {'LineWidth', value}];
      case {'displayname', 'visible', 'tag', 'userdata', 'handlevisibility'}
        chartProps = [chartProps, {name, value}];
      case 'sourcetable'
        chartProps = [chartProps, {'SourceTable', value}];
      case 'thetavariable'
        chartProps = [chartProps, {'ThetaVariable', value}];
      case 'rvariable'
        chartProps = [chartProps, {'RVariable', value}];
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'polarscatter');
    end
  end
end
%=============================================================================
function [theta, radius, args, tableArgs] = parseTableScatterData(args)
  tbl = args{1};
  if numel(args) < 3
    error('Nelson:graphics:tableVariableExpected', 'Table variable arguments expected.');
  end
  thetaNames = graphicsTableVariableNames(tbl, args{2});
  radiusNames = graphicsTableVariableNames(tbl, args{3});
  args = args(4:end);
  nSeries = max(numel(thetaNames), numel(radiusNames));
  validateTableVariableCounts(thetaNames, radiusNames, nSeries);
  [theta, radius, tableArgs] = tableScatterSeries(tbl, thetaNames, radiusNames, nSeries);
end
%=============================================================================
function validateTableVariableCounts(thetaNames, radiusNames, nSeries)
  counts = [numel(thetaNames), numel(radiusNames)];
  if any(counts ~= 1 & counts ~= nSeries)
    error('Nelson:dimagree:tableVariableCountsMatch', 'Table variable counts must match.');
  end
end
%=============================================================================
function [theta, radius, tableArgs] = tableScatterSeries(tbl, thetaNames, radiusNames, nSeries)
  theta = [];
  radius = [];
  tableArgs = cell(1, nSeries);
  for k = 1:nSeries
    thetaName = graphicsSelectName(thetaNames, k);
    radiusName = graphicsSelectName(radiusNames, k);
    theta = graphicsAppendColumn(theta, graphicsNumericColumn(tbl.(thetaName), thetaName));
    radius = graphicsAppendColumn(radius, graphicsNumericColumn(tbl.(radiusName), radiusName));
    oneArgs = {'SourceTable', tbl, 'ThetaVariable', thetaName, 'RVariable', radiusName};
    if nSeries > 1
      oneArgs(end + 1:end + 2) = {'DisplayName', radiusName};
    end
    tableArgs{k} = oneArgs;
  end
end
%=============================================================================
function [orientation, nSeries] = getSeriesOrientation(theta, radius)
  if isvector(theta) && isvector(radius)
    orientation = 'single';
    nSeries = 1;
    return
  end
  if isvector(theta) && ~isvector(radius)
    if numel(theta) == size(radius, 1)
      orientation = 'columns';
      nSeries = size(radius, 2);
      return
    end
  end
  if ~isvector(theta) && isvector(radius)
    if numel(radius) == size(theta, 1)
      orientation = 'columns';
      nSeries = size(theta, 2);
      return
    end
  end
  if isequal(size(theta), size(radius))
    orientation = 'columns';
    nSeries = size(theta, 2);
    return
  end
  error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'Theta', 'radius');
end
%=============================================================================
