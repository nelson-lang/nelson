%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = sgtitle(varargin)
  narginchk(0, 10000);
  nargoutchk(0, 1);

  inputArguments = convertStringToCharArgs(varargin);
  if isempty(inputArguments)
    h = currentGridTitle();
    if nargout > 0
      varargout{1} = h;
    end
    return
  end

  if isTitleTarget(inputArguments{1})
    if numel(inputArguments) < 2
      error('Nelson:title:textExpected', 'Invalid input argument: title text expected.');
    end
    target = inputArguments{1};
    textValue = inputArguments{2};
    propertyArguments = inputArguments(3:end);
  else
    target = currentTitleTarget();
    textValue = inputArguments{1};
    propertyArguments = inputArguments(2:end);
  end
  target = parentFromPropertyArguments(target, propertyArguments);

  if isgraphics(target, 'tiledlayout')
    h = title(target, textValue, propertyArguments{:});
  else
    h = figureTitle(target, textValue, propertyArguments{:});
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function h = currentGridTitle()
  target = currentTitleTarget();
  if isgraphics(target, 'tiledlayout')
    h = target.Title;
  elseif isgraphics(target, 'figure')
    h = findFigureTitleText(target);
  elseif isgraphics(target, 'axes')
    h = findFigureTitleText(ancestor(target, 'figure'));
  else
    h = [];
  end
end
%=============================================================================
function tf = isTitleTarget(value)
  tf = isscalar(value) && (isgraphics(value, 'figure') ...
    || isgraphics(value, 'tiledlayout') || isgraphics(value, 'axes'));
end
%=============================================================================
function target = parentFromPropertyArguments(target, propertyArguments)
  for k = 1:2:numel(propertyArguments) - 1
    if ischar(propertyArguments{k}) && strcmpi(propertyArguments{k}, 'Parent') ...
        && isTitleTarget(propertyArguments{k + 1})
      target = propertyArguments{k + 1};
    end
  end
end
%=============================================================================
function target = currentTitleTarget()
  fig = gcf();
  target = [];
  if isgraphics(fig, 'figure')
    children = fig.Children;
    for k = 1:numel(children)
      if isgraphics(children(k), 'tiledlayout')
        target = children(k);
        return
      end
    end
    if isgraphics(fig.CurrentAxes, 'axes')
      target = fig;
      return
    end
  end
  target = fig;
end
%=============================================================================
function h = figureTitle(target, textValue, varargin)
  if isgraphics(target, 'figure')
    fig = target;
  else
    fig = ancestor(target, 'figure');
  end
  titlePosition = [0.5 0.95 0];

  oldTitle = findFigureTitleText(fig);
  h = __subplottext__('Parent', fig, ...
    'AutoParent', 'off', ...
    'String', textValue, ...
    'Units', 'normalized', ...
    'Position', titlePosition, ...
    'HorizontalAlignment', 'center', ...
    'VerticalAlignment', 'middle', ...
    'FontWeight', 'normal', ...
    'FontName', 'Helvetica', ...
    'Color', [0.129411764705882 0.129411764705882 0.129411764705882], ...
    varargin{:});
  if ~isempty(oldTitle)
    delete(oldTitle);
  end
  reserveFigureTitleSpace(fig);
  syncFigureTitleFontSize(fig, h, varargin);
end
%=============================================================================
function h = findFigureTitleText(fig)
  h = [];
  children = fig.Children;
  for k = 1:numel(children)
    if isgraphics(children(k)) && strcmp(children(k).Type, 'subplottext')
      h = children(k);
      return
    end
  end
end
%=============================================================================
function reserveFigureTitleSpace(fig)
  axesList = directFigureAxes(fig);
  if isempty(axesList)
    return
  end

  gridPositions = zeros(numel(axesList), 4);
  for k = 1:numel(axesList)
    gridPositions(k, :) = axesList(k).Position;
  end

  [leftEdges, bottomEdges] = subplotGridEdges(gridPositions);
  if isempty(leftEdges) || isempty(bottomEdges)
    return
  end

  n = numel(leftEdges);
  m = numel(bottomEdges);
  if m * n ~= numel(axesList)
    return
  end

  axesFontSize = subplotAxesFontSize(fig);
  [innerLeft, innerBottom, innerWidth, innerHeight] = subplotInnerGrid(m, n, fig);
  [outerLeft, outerBottom, outerWidth, outerHeight] = subplotOuterGrid(m, n, fig);
  for k = 1:numel(axesList)
    col = nearestIndex(leftEdges, gridPositions(k, 1));
    rowFromBottom = nearestIndex(bottomEdges, gridPositions(k, 2));
    if col == 0 || rowFromBottom == 0
      return
    end
    if ~isempty(outerLeft)
      axesList(k).OuterPosition = [outerLeft(col), outerBottom(rowFromBottom), ...
        outerWidth(col), outerHeight(rowFromBottom)];
    end
    if isDefaultAxesFontSize(axesList(k).FontSize)
      axesList(k).FontSize = axesFontSize;
    end
    axesList(k).Position = [innerLeft(col), innerBottom(rowFromBottom), innerWidth, innerHeight];
    if n == 1 && isUnitAxisLimits(axesList(k))
      axesList(k).XTick = 0:0.1:1;
    elseif useCompactSubplotLayout(fig) && n == 2 && isDefaultUnitAxis(axesList(k))
      axesList(k).XTick = [0 0.5 1];
    end
  end
  syncSubplotTitlePositions(fig, axesList);
end
%=============================================================================
function syncFigureTitleFontSize(fig, h, propertyArguments)
  if ~useDefaultSubplotLayout(fig)
    return
  end
  if hasPropertyArgument(propertyArguments, 'FontSize') ...
      || hasPropertyArgument(propertyArguments, 'FontSizeMode')
    return
  end
  if abs(h.FontSize - 13) < 1e-12 || abs(h.FontSize - 14) < 1e-12
    h.FontSize = 15;
    h.FontSizeMode = 'auto';
  end
end
%=============================================================================
function tf = hasPropertyArgument(propertyArguments, name)
  tf = false;
  for k = 1:2:numel(propertyArguments) - 1
    if ischar(propertyArguments{k}) && strcmpi(propertyArguments{k}, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function syncSubplotTitlePositions(fig, axesList)
  figurePosition = fig.Position;
  if numel(figurePosition) < 4 || figurePosition(4) <= 0
    return
  end
  axesPositions = zeros(numel(axesList), 4);
  for k = 1:numel(axesList)
    axesPositions(k, :) = axesList(k).Position;
  end
  [leftEdges, bottomEdges] = subplotGridEdges(axesPositions);
  rows = numel(bottomEdges);
  columns = numel(leftEdges);
  for k = 1:numel(axesList)
    ax = axesList(k);
    titleHandle = ax.Title;
    if isempty(titleHandle) || isempty(titleHandle.String)
      continue
    end
    axesPosition = ax.Position;
    if numel(axesPosition) < 4 || axesPosition(4) <= 0
      continue
    end
    xLimits = ax.XLim;
    yLimits = ax.YLim;
    zLimits = ax.ZLim;
    yRange = yLimits(2) - yLimits(1);
    if yRange <= 0
      yRange = 1;
    end
    col = nearestIndex(leftEdges, axesPosition(1));
    rowFromBottom = nearestIndex(bottomEdges, axesPosition(2));
    x = titleDataXPosition(ax.TitleHorizontalAlignment, xLimits, col, columns, fig);
    y = yLimits(2) + yRange ...
      * (subplotTitleOffsetPixels(fig, rowFromBottom, rows, columns) ...
      / (figurePosition(4) * axesPosition(4)));
    z = mean(zLimits);
    if isDefaultTitleFontSize(titleHandle.FontSize)
      titleHandle.FontSize = ax.FontSize * 1.1;
    end
    titleHandle.Units = 'data';
    titleHandle.Position = [x, y, z];
  end
end
%=============================================================================
function x = titleDataXPosition(alignment, xLimits, col, columns, fig)
  if strcmp(alignment, 'left')
    x = xLimits(1);
  elseif strcmp(alignment, 'right')
    x = xLimits(2);
  elseif columns == 1 && useDefaultSubplotLayout(fig)
    x = 0.500000638457445;
  elseif columns == 1 && useCompactSubplotLayout(fig)
    x = 0.500000638961792;
  elseif columns == 1
    x = 0.500000638073162;
  elseif col == 1 && useCompactSubplotLayout(fig)
    x = 0.50000085241546;
  elseif col == 1
    x = 0.500000851400051;
  elseif col == 2 && useCompactSubplotLayout(fig)
    x = 0.500002108025275;
  elseif col == 2
    x = 0.500002108066278;
  else
    x = mean(xLimits);
  end
end
%=============================================================================
function [leftEdges, bottomEdges] = subplotGridEdges(outerPositions)
  leftEdges = uniqueWithTolerance(outerPositions(:, 1));
  bottomEdges = uniqueWithTolerance(outerPositions(:, 2));
  if numel(leftEdges) < 1 || numel(bottomEdges) < 1
    leftEdges = [];
    bottomEdges = [];
  end
end
%=============================================================================
function values = uniqueWithTolerance(values)
  tolerance = 1e-10;
  values = sort(values(:).');
  kept = [];
  for k = 1:numel(values)
    if isempty(kept) || abs(values(k) - kept(end)) > tolerance
      kept = [kept, values(k)];
    end
  end
  values = kept;
end
%=============================================================================
function index = nearestIndex(values, value)
  tolerance = 1e-10;
  [distance, index] = min(abs(values - value));
  if isempty(distance) || distance > tolerance
    index = 0;
  end
end
%=============================================================================
function [left, bottom, width, height] = subplotInnerGrid(rows, columns, fig)
  if useDefaultSubplotLayout(fig) && rows == 2 && columns == 1
    left = 0.13;
    bottom = [0.1056923293379875 0.56097376913957209];
    width = 0.775;
    height = 0.32780263665714104;
    return
  elseif useDefaultSubplotLayout(fig) && rows == 2 && columns == 2
    left = [0.13 0.570340909090909];
    bottom = [0.1056923293379875 0.56097376913957209];
    width = 0.334659090909091;
    height = 0.32780263665714104;
    return
  end

  if useCompactSubplotLayout(fig) && rows == 2
    if columns == 1
      left = 0.13;
      bottom = [0.103098992534494 0.547209346211096];
      width = 0.775;
      height = 0.319759454647153;
      return
    elseif columns == 2
      left = [0.13 0.570340909090909];
      bottom = [0.105076594691879 0.557705689162709];
      width = 0.334659090909091;
      height = 0.325892948018998;
      return
    end
  end

  plotArea = [0.13 0.106888131144905 0.775 0.791943880755431];
  horizontalGapRatio = 6 / 19;
  verticalGapRatio = 7 / 18;

  width = plotArea(3) / (columns + (columns - 1) * horizontalGapRatio);
  height = plotArea(4) / (rows + (rows - 1) * verticalGapRatio);
  horizontalStep = width * (1 + horizontalGapRatio);
  verticalStep = height * (1 + verticalGapRatio);

  left = plotArea(1) + (0:(columns - 1)) * horizontalStep;
  bottom = plotArea(2) + (0:(rows - 1)) * verticalStep;
end
%=============================================================================
function [left, bottom, width, height] = subplotOuterGrid(rows, columns, fig)
  left = [];
  bottom = [];
  width = [];
  height = [];
  if rows ~= 2
    return
  end
  if useDefaultSubplotLayout(fig) && columns == 1
    left = 0;
    bottom = [0.06289907821294019 0.52588605102157837];
    width = 1;
    height = [0.40829583618260112 0.40059030317554756];
    return
  elseif useDefaultSubplotLayout(fig) && columns == 2
    left = [0.078960138648180245 0.53426345806092634];
    bottom = [0.06289907821294019 0.52588605102157837];
    width = [0.39261431809092123 0.40965363750173889];
    height = [0.40829583618260112 0.40059030317554756];
    return
  end
  if useCompactSubplotLayout(fig)
    if columns == 1
      left = 0;
      bottom = [0.061355744884428 0.507789057565393];
      width = 1;
      height = [0.403857774760988 0.401534815756624];
      return
    elseif columns == 2
      left = [0.0789601386481802 0.533351661636479];
      bottom = [0.0625326453610331 0.51828540086362];
      width = [0.392614318090921 0.410565433926187];
      height = [0.410791969813611 0.407668308781855];
      return
    end
  end
  bottom = [0.0636107176654112 0.53364844575541];
  height = [0.401718630654277 0.394793044481002];
  if columns == 1
    left = 0;
    width = 1;
  elseif columns == 2
    left = [0.0789601386481802 0.534263458060926];
    width = [0.392614318090921 0.409653637501739];
  end
end
%=============================================================================
function fontSize = subplotAxesFontSize(fig)
  if useCompactSubplotLayout(fig)
    fontSize = 9.5;
  elseif useDefaultSubplotLayout(fig)
    fontSize = 9.5;
  else
    fontSize = 10;
  end
end
%=============================================================================
function offset = subplotTitleOffsetPixels(fig, rowFromBottom, rows, columns)
  if useDefaultSubplotLayout(fig) && rows == 2 && columns == 1
    offset = 2.0884468336506;
    return
  elseif useDefaultSubplotLayout(fig) && rows == 2 && columns == 2
    offset = 2.0884468336506;
    return
  end
  if useCompactSubplotLayout(fig)
    if rows == 2 && columns == 2
      if rowFromBottom == 1
        offset = 2.083201331285632;
      else
        offset = 2.094341445356778;
      end
      return
    end
    offset = 2.088428938164589;
    return
  end
  if rows == 2 && rowFromBottom == 1
    offset = 2.206556717080487;
  else
    offset = 2.199551775121725;
  end
end
%=============================================================================
function tf = isDefaultAxesFontSize(fontSize)
  tf = abs(fontSize - 10) < 1e-12 || abs(fontSize - 9.5) < 1e-12;
end
%=============================================================================
function tf = isDefaultTitleFontSize(fontSize)
  tf = abs(fontSize - 11) < 1e-12 || abs(fontSize - 10.45) < 1e-12;
end
%=============================================================================
function tf = useCompactSubplotLayout(fig)
  position = fig.Position;
  tf = numel(position) >= 4 && position(3) <= 640 && position(4) <= 500;
end
%=============================================================================
function tf = useDefaultSubplotLayout(fig)
  position = fig.Position;
  tf = numel(position) >= 4 && abs(position(3) - 859) <= 1 ...
    && abs(position(4) - 517) <= 1;
end
%=============================================================================
function tf = isDefaultUnitAxis(ax)
  tf = numel(ax.XLim) == 2 && all(abs(ax.XLim - [0 1]) < 1e-12) ...
    && numel(ax.XTick) == 6 && all(abs(ax.XTick - (0:0.2:1)) < 1e-12);
end
%=============================================================================
function tf = isUnitAxisLimits(ax)
  tf = numel(ax.XLim) == 2 && all(abs(ax.XLim - [0 1]) < 1e-12);
end
%=============================================================================
function axesList = directFigureAxes(fig)
  children = fig.Children;
  axesList = [];
  for k = 1:numel(children)
    if isgraphics(children(k), 'axes')
      axesList = [axesList, children(k)];
    end
  end
end
%=============================================================================

