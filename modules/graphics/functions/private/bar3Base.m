%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = bar3Base(horizontal, varargin)
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [data, rowPositions, args, explicitPositions] = parseBar3Data(args);
  opts = parseBar3Options(args);
  ax = graphicsTargetAxes(parent);
  if isempty(parent)
    parent = ax;
  end
  h = drawBars(parent, ax, data, rowPositions, horizontal, opts);
  applyBar3Axes(ax, data, rowPositions, explicitPositions, horizontal, opts);
end
%=============================================================================
function [data, rowPositions, args, explicitPositions] = parseBar3Data(args)
  if isempty(args) || ~isnumeric(args{1}) || ~isreal(args{1})
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  explicitPositions = false;
  if numel(args) >= 2 && isExplicitPositionSyntax(args{1}, args{2})
    rowPositions = double(args{1}(:));
    data = double(args{2});
    args = args(3:end);
    explicitPositions = true;
  else
    data = double(args{1});
    args = args(2:end);
  end
  if isvector(data)
    data = data(:);
  end
  if ~exist('rowPositions', 'var')
    rowPositions = (1:size(data, 1)).';
  end
  if numel(rowPositions) ~= size(data, 1)
    error('Nelson:bar3Base:invalidPositionVectorLength', 'Position vector length must match data rows.');
  end
  if any(~isfinite(rowPositions))
    error('Nelson:bar3Base:invalidPositionVector', 'Position vector must contain finite values.');
  end
end
%=============================================================================
function tf = isExplicitPositionSyntax(positions, data)
  tf = isnumeric(data) && isreal(data) && isvector(positions) && ~isscalar(data);
end
%=============================================================================
function opts = parseBar3Options(args)
  opts = struct('BarWidth', 0.8, 'Style', 'detached', 'FaceColor', 'flat', ...
    'EdgeColor', repmat(33 / 255, 1, 3), 'FaceAlpha', 1, 'LineStyle', '-', 'LineWidth', 0.5);
  if ~isempty(args) && isnumeric(args{1}) && isscalar(args{1})
    opts.BarWidth = double(args{1});
    args = args(2:end);
  end
  if ~isempty(args) && isText(args{1})
    value = char(args{1});
    if any(strcmpi(value, {'detached', 'grouped', 'stacked'}))
      opts.Style = lower(value);
      args = args(2:end);
    elseif isColorString(value)
      opts.FaceColor = getColorShortName(value);
      args = args(2:end);
    end
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case {'barwidth', 'width'}
        opts.BarWidth = double(value);
      case 'facecolor'
        opts.FaceColor = value;
      case 'edgecolor'
        opts.EdgeColor = value;
      case 'facealpha'
        opts.FaceAlpha = double(value);
      case 'linestyle'
        opts.LineStyle = value;
      case 'linewidth'
        opts.LineWidth = double(value);
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'bar3');
    end
  end
  if ~isscalar(opts.BarWidth) || ~isfinite(opts.BarWidth) || opts.BarWidth <= 0
    error('Nelson:graphics:mustBePositiveScalar', '%s must be a positive scalar.', 'BarWidth');
  end
end
%=============================================================================
function h = drawBars(parent, ax, data, rowPositions, horizontal, opts)
  [rows, cols] = size(data);
  h = gobjects(1, cols);
  bases = zeros(rows, cols);
  if strcmp(opts.Style, 'stacked')
    bases = stackedBases(data);
  end
  for col = 1:cols
    [xData, yData, zData] = surfaceDataForColumn(data, bases, rowPositions, col, horizontal, opts);
    cData = col * ones(size(zData));
    h(col) = surface('Parent', parent, 'XData', xData, 'YData', yData, ...
      'ZData', zData, 'CData', cData, 'FaceColor', opts.FaceColor, ...
      'EdgeColor', opts.EdgeColor, 'FaceAlpha', opts.FaceAlpha, ...
      'LineStyle', opts.LineStyle, 'LineWidth', opts.LineWidth, ...
      'FaceLighting', 'flat', 'BackFaceLighting', 'reverselit');
  end
end
%=============================================================================
function bases = stackedBases(data)
  [rows, cols] = size(data);
  bases = zeros(rows, cols);
  running = zeros(rows, 1);
  for col = 1:cols
    bases(:, col) = running;
    running = running + data(:, col);
  end
end
%=============================================================================
function [xData, yData, zData] = surfaceDataForColumn(data, bases, rowPositions, col, horizontal, opts)
  [rows, cols] = size(data);
  xData = [];
  yData = [];
  zData = [];
  for row = 1:rows
    [xlim, positionLimits] = barFootprint(row, col, cols, rowPositions, opts);
    base = bases(row, col);
    top = base + data(row, col);
    [xBlock, yBlock, zBlock] = cuboidBlock(xlim, positionLimits, base, top, horizontal);
    xData = [xData; xBlock];
    yData = [yData; yBlock];
    zData = [zData; zBlock];
  end
end
%=============================================================================
function [xlim, positionLimits] = barFootprint(row, col, cols, rowPositions, opts)
  width = opts.BarWidth;
  positionUnit = rowPositionUnit(rowPositions);
  switch opts.Style
    case 'grouped'
      groupWidth = automaticGroupWidth(width, cols);
      xSpan = groupWidth * width / cols;
      positionSpan = xSpan * positionUnit;
      offset = (col - (cols + 1) / 2) * groupWidth * positionUnit / cols;
      xlim = [1 - xSpan / 2, 1 + xSpan / 2];
      center = rowPositions(row) + offset;
      positionLimits = [center - positionSpan / 2, center + positionSpan / 2];
    case 'stacked'
      xlim = [1 - width / 2, 1 + width / 2];
      span = width * positionUnit;
      positionLimits = [rowPositions(row) - span / 2, rowPositions(row) + span / 2];
    otherwise
      xlim = [col - width / 2, col + width / 2];
      span = width * positionUnit;
      positionLimits = [rowPositions(row) - span / 2, rowPositions(row) + span / 2];
  end
end
%=============================================================================
function unit = rowPositionUnit(rowPositions)
  if numel(rowPositions) < 2
    unit = 1;
    return
  end
  steps = abs(diff(rowPositions(:).'));
  steps = steps(steps > 0);
  if isempty(steps)
    unit = 1;
  else
    unit = min(steps);
  end
end
%=============================================================================
function groupWidth = automaticGroupWidth(width, cols)
  if cols <= 1
    groupWidth = width;
  else
    groupWidth = min(width, cols / (cols + 1.5));
  end
end
%=============================================================================
function [xBlock, yBlock, zBlock] = cuboidBlock(xlim, positionLimits, base, top, horizontal)
  xBlock = [NaN xlim(1) xlim(2) NaN; xlim(1) xlim(1) xlim(2) xlim(2); ...
    xlim(1) xlim(1) xlim(2) xlim(2); NaN xlim(1) xlim(2) NaN; ...
    NaN xlim(1) xlim(2) NaN; NaN NaN NaN NaN];
  pBlock = [NaN positionLimits(1) positionLimits(1) NaN; ...
    positionLimits(1) positionLimits(1) positionLimits(1) positionLimits(1); ...
    positionLimits(2) positionLimits(2) positionLimits(2) positionLimits(2); ...
    NaN positionLimits(2) positionLimits(2) NaN; ...
    NaN positionLimits(1) positionLimits(1) NaN; NaN NaN NaN NaN];
  vBlock = [NaN base base NaN; base top top base; base top top base; ...
    NaN base base NaN; NaN base base NaN; NaN NaN NaN NaN];
  if horizontal
    yBlock = vBlock;
    zBlock = pBlock;
  else
    yBlock = pBlock;
    zBlock = vBlock;
  end
end
%=============================================================================
function applyBar3Axes(ax, data, rowPositions, explicitPositions, horizontal, opts)
  if isempty(ax)
    return
  end
  [rows, cols] = size(data);
  valueFootprint = bar3ValueFootprintLimits(data, opts);
  if horizontal
    positionLimits = mapRowLimits(horizontalRowLimits(rows, opts, explicitPositions), rowPositions, explicitPositions);
    valueLimits = horizontalValueAxisLimits(data, opts);
  else
    positionLimits = mapRowLimits(rowLimits(rows, opts, explicitPositions, valueFootprint), rowPositions, explicitPositions);
    valueLimits = valueAxisLimits(data, opts);
  end
  xLimits = xAxisLimits(cols, opts);
  [~, positionFootprint] = bar3FootprintLimits(data, rowPositions, opts);
  if needsBar3LimitsPadding()
    if horizontal
      valueLimits = includeValueLimitsWithPadding(valueLimits, valueFootprint, ...
        bar3ValuePadding(valueFootprint));
    else
      positionLimits = includeLimitsWithPadding(positionLimits, positionFootprint, ...
        bar3PositionPadding(rowPositions, valueFootprint, opts));
    end
  end
  if horizontal
    ax.XLim = xLimits;
    ax.YLim = valueLimits;
    ax.ZLim = positionLimits;
  else
    ax.XLim = xLimits;
    ax.YLim = positionLimits;
    ax.ZLim = valueLimits;
  end
  applyBar3Aspect(ax, xLimits, rows, horizontal);
  applyBar3Ticks(ax, rowPositions, cols, horizontal, opts);
  applyBar3ColorLimits(ax, cols);
  ax.SortMethod = 'depth';
  view(ax, 3);
  applyBar3Camera(ax, horizontal, opts);
  grid(ax, 'on');
end
%=============================================================================
function limits = mapRowLimits(indexLimits, rowPositions, explicitPositions)
  if numel(rowPositions) < 2
    step = 1;
  else
    step = rowPositionUnit(rowPositions);
  end
  mapped = rowPositions(1) + (indexLimits - 1) * step;
  limits = [min(mapped), max(mapped)];
  if explicitPositions && needsAutomaticPositionLimits(rowPositions)
    limits = niceAxisLimits(limits);
  end
end
%=============================================================================
function tf = needsAutomaticPositionLimits(rowPositions)
  defaultPositions = (1:numel(rowPositions)).';
  tf = numel(rowPositions) > 15 && any(rowPositions(:) ~= defaultPositions);
end
%=============================================================================
function limits = niceAxisLimits(limits)
  span = diff(limits);
  if ~isfinite(span) || span <= 0
    return
  end
  step = niceAxisStep(span / 7);
  limits = [floor(limits(1) / step) * step, ceil(limits(2) / step) * step];
end
%=============================================================================
function step = niceAxisStep(value)
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
function applyBar3ColorLimits(ax, cols)
  % Each series surface carries a uniform CData equal to its column index
  % (1..cols). A uniform CData reports a degenerate color range per surface, so
  % the axis auto CLim scan cannot recover the overall [1 cols] span on its own.
  % Set it explicitly, keeping a floor of 2 so a single series stays [1 2].
  ax.CLim = [1, max(cols, 2)];
end
%=============================================================================
function applyBar3Aspect(ax, xLimits, rows, horizontal)
  % The plot-box x extent is the column-axis range itself (diff of the axis
  % limits), not a padded footprint: padding x here would stretch the box
  % sideways and flatten the bars relative to their height.
  dx = diff(xLimits);
  if horizontal
    dz = rows + 1;
    dy = (sqrt(5) - 1) / 2 * dz;
  else
    dy = rows + 1;
    dz = (sqrt(5) - 1) / 2 * dy;
  end
  ax.PlotBoxAspectRatio = [dx dy dz];
  ax.YDir = 'reverse';
end
%=============================================================================
function applyBar3Camera(ax, horizontal, opts)
  if needsBar3StackedUnzoom(horizontal, opts)
    ax.CameraViewAngle = ax.CameraViewAngle * bar3StackedUnzoomFactor();
  end
end
%=============================================================================
function tf = needsBar3StackedUnzoom(horizontal, opts)
  tf = ~horizontal && strcmp(opts.Style, 'stacked');
end
%=============================================================================
function factor = bar3StackedUnzoomFactor()
  factor = 1 / (0.95 ^ 3);
end
%=============================================================================
function tf = needsBar3LimitsPadding()
  tf = false;
end
%=============================================================================
function [xLimits, positionLimits] = bar3FootprintLimits(data, rowPositions, opts)
  [rows, cols] = size(data);
  xMin = Inf;
  xMax = -Inf;
  positionMin = Inf;
  positionMax = -Inf;
  for col = 1:cols
    for row = 1:rows
      [xFootprint, positionFootprint] = barFootprint(row, col, cols, rowPositions, opts);
      xMin = min(xMin, xFootprint(1));
      xMax = max(xMax, xFootprint(2));
      positionMin = min(positionMin, positionFootprint(1));
      positionMax = max(positionMax, positionFootprint(2));
    end
  end
  xLimits = [xMin, xMax];
  positionLimits = [positionMin, positionMax];
end
%=============================================================================
function limits = bar3ValueFootprintLimits(data, opts)
  if strcmp(opts.Style, 'stacked')
    values = [zeros(size(data, 1), 1), cumsum(data, 2)];
  else
    values = [0; data(:)];
  end
  values = values(isfinite(values));
  if isempty(values)
    limits = [0 1];
  else
    limits = [min(values(:)), max(values(:))];
  end
end
%=============================================================================
function padding = bar3ValuePadding(valueFootprint)
  padding = 0.1 * diff(valueFootprint);
end
%=============================================================================
function padding = bar3PositionPadding(rowPositions, valueFootprint, opts)
  if crossesZero(valueFootprint)
    ratio = 0.4;
  elseif strcmp(opts.Style, 'grouped')
    ratio = 0.635;
  else
    ratio = 0.26;
  end
  padding = ratio * rowPositionUnit(rowPositions);
end
%=============================================================================
function tf = crossesZero(limits)
  tf = limits(1) < 0 && limits(2) > 0;
end
%=============================================================================
function limits = includeLimitsWithPadding(limits, footprint, padding)
  limits = [min(limits(1), footprint(1) - padding), max(limits(2), footprint(2) + padding)];
end
%=============================================================================
function limits = includeValueLimitsWithPadding(limits, footprint, padding)
  if footprint(1) < 0
    limits(1) = min(limits(1), footprint(1) - padding);
  end
  if footprint(2) > 0
    limits(2) = max(limits(2), footprint(2) + padding);
  end
end
%=============================================================================
function applyBar3Ticks(ax, rowPositions, cols, horizontal, opts)
  columnTicks = columnTickValues(cols, opts);
  rowTicks = rowTickValues(rowPositions);
  if horizontal
    ax.XTick = columnTicks;
    valueTicks = horizontalValueTicks(ax.YLim);
    if ~isempty(valueTicks)
      ax.YTick = valueTicks;
    end
    if ~isempty(rowTicks)
      ax.ZTick = rowTicks;
    end
  else
    ax.XTick = columnTicks;
    valueTicks = verticalValueTicks(ax.ZLim);
    if ~isempty(valueTicks)
      ax.ZTick = valueTicks;
    end
    if ~isempty(rowTicks)
      ax.YTick = rowTicks;
    end
  end
end
%=============================================================================
function ticks = columnTickValues(cols, opts)
  if strcmp(opts.Style, 'detached') && cols > 1
    ticks = 1:cols;
  else
    ticks = [];
  end
end
%=============================================================================
function ticks = rowTickValues(rowPositions)
  rows = numel(rowPositions);
  if rows <= 15
    ticks = rowPositions(:).';
  else
    ticks = [];
  end
end
%=============================================================================
function limits = xAxisLimits(cols, opts)
  width = opts.BarWidth;
  switch opts.Style
    case 'grouped'
      limits = [1 - width / (2 * cols), 1 + width / (2 * cols)];
    case 'stacked'
      limits = [1 - width / 2, 1 + width / 2];
    otherwise
      limits = [1 - width / 2, cols + width / 2];
  end
end
%=============================================================================
function limits = horizontalRowLimits(rows, opts, explicitPositions)
  if (explicitPositions && rows >= 3) || rows >= 3 || (strcmp(opts.Style, 'grouped') && rows >= 3)
    limits = [0.5, rows + 0.5];
  else
    width = opts.BarWidth;
    limits = [1 - width / 2, rows + width / 2];
  end
end
%=============================================================================
function limits = horizontalValueAxisLimits(data, opts)
  limits = valueAxisLimits(data, opts);
  if limits(1) == 0 && limits(2) > 0
    limits(2) = nicePositiveUpper(limits(2));
  end
end
%=============================================================================
function upper = nicePositiveUpper(value)
  scale = 10 ^ floor(log10(value));
  normalized = value / scale;
  if normalized <= 1
    nice = 1;
  elseif normalized <= 1.5
    nice = 1.5;
  elseif normalized <= 2
    nice = 2;
  elseif normalized < 4
    nice = 4;
  elseif normalized <= 5
    nice = 5;
  elseif normalized <= 6
    nice = 6;
  elseif normalized <= 8
    nice = 8;
  else
    nice = 10;
  end
  upper = nice * scale;
end
%=============================================================================
function ticks = verticalValueTicks(limits)
  if numel(limits) ~= 2
    ticks = [];
    return
  end
  if crossesZero(limits) && diff(limits) > 10
    step = niceAxisStep(diff(limits) / 7);
    ticks = limits(1):step:limits(2);
    return
  end
  if limits(1) ~= 0 || limits(2) <= 0
    ticks = [];
    return
  end
  upper = limits(2);
  scale = 10 ^ floor(log10(upper));
  normalized = upper / scale;
  if normalized <= 2
    step = scale / 5;
  elseif normalized <= 6
    step = scale;
  else
    step = 2 * scale;
  end
  ticks = 0:step:upper;
end
%=============================================================================
function ticks = horizontalValueTicks(limits)
  if numel(limits) ~= 2 || limits(1) ~= 0 || limits(2) <= 0
    ticks = [];
    return
  end
  upper = limits(2);
  scale = 10 ^ floor(log10(upper));
  normalized = upper / scale;
  if normalized <= 1
    step = scale / 5;
  elseif normalized <= 2
    step = 0.5 * scale;
  elseif normalized < 6
    step = scale;
  else
    step = 2 * scale;
  end
  ticks = 0:step:upper;
end
%=============================================================================
function limits = rowLimits(rows, opts, explicitPositions, valueFootprint)
  if rows >= 15 || (rows >= 5 && crossesZero(valueFootprint) && ~strcmp(opts.Style, 'stacked'))
    limits = [0, rowLimitUpper(rows)];
    return
  end
  if rows >= 3 && (strcmp(opts.Style, 'detached') || strcmp(opts.Style, 'grouped'))
    limits = [0.5, rows + 0.5];
  elseif strcmp(opts.Style, 'stacked') && opts.BarWidth < 0.8
    limits = [0.5, rows + 0.5];
  else
    width = opts.BarWidth;
    limits = [1 - width / 2, rows + width / 2];
  end
end
%=============================================================================
function upper = rowLimitUpper(rows)
  if rows < 10
    upper = rows + 1;
  elseif rows < 14
    upper = 2 * ceil((rows + 0.5) / 2);
  elseif rows < 16
    upper = rows + 1;
  elseif rows < 20
    upper = 2 * ceil((rows + 0.5) / 2);
  else
    upper = 5 * ceil((rows + 0.5) / 5);
  end
end
%=============================================================================
function limits = valueAxisLimits(data, opts)
  if strcmp(opts.Style, 'stacked')
    values = [zeros(size(data, 1), 1), cumsum(data, 2)];
    values = values(:);
  else
    values = [0; data(:)];
  end
  values = values(isfinite(values));
  if isempty(values)
    limits = [0 1];
    return
  end
  limits = [min(values), max(values)];
  if limits(1) == limits(2)
    limits = [limits(1) - 0.5, limits(2) + 0.5];
  end
  if ~strcmp(opts.Style, 'stacked') && crossesZero(limits)
    limits = niceAxisLimits(limits);
  end
end
%=============================================================================
function tf = isColorString(value)
  names = [getColorShortNameList(), getColorNameList()];
  tf = any(strcmpi(value, names));
end
%=============================================================================
function tf = isText(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
