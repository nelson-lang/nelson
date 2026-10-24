%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = stem3(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseStem3Parent(args);
  [x, y, z, filled, lineSpec, propertyPairs, implicitColumnCount] = parseStem3Arguments(args);
  if isempty(parent)
    parent = graphicsParentProperty(propertyPairs);
  end
  parent = newplotTarget(parent);
  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
  end
  group = drawStem3(ax, parent, x, y, z, filled, lineSpec, propertyPairs);
  applyStem3ImplicitAxesState(ax, implicitColumnCount);
  if nargout == 1
    varargout{1} = group;
  end
end
%=============================================================================
function [parent, args] = parseStem3Parent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [x, y, z, filled, lineSpec, propertyPairs, implicitColumnCount] = parseStem3Arguments(args)
  firstStyle = firstStringArgument(args);
  implicitColumnCount = [];
  if firstStyle == 1
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if firstStyle == 2
    z = args{1};
    if ~isvector(z)
      implicitColumnCount = size(z, 2);
    end
    [x, y] = meshgrid(1:size(z, 2), 1:size(z, 1));
    args = args(2:end);
  elseif firstStyle >= 4
    x = args{1};
    y = args{2};
    z = args{3};
    args = args(4:end);
  else
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if ~isnumeric(x) || ~isnumeric(y) || ~isnumeric(z)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  [x, y, z] = normalizeStem3Data(x, y, z);
  filled = false;
  remove = [];
  for k = 1:length(args)
    if isTextScalar(args{k}) && strcmp(convertStringsToChars(args{k}), 'filled')
      filled = true;
      remove = [remove, k];
    end
  end
  args(remove) = [];
  lineSpec = '-o';
  if ~isempty(args) && isTextScalar(args{1}) && ~isValidGraphicsProperty('line', args{1})
    lineSpec = convertStringsToChars(args{1});
    args = args(2:end);
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  propertyPairs = args;
end
%=============================================================================
function index = firstStringArgument(args)
  index = length(args) + 1;
  for k = 1:length(args)
    if isTextScalar(args{k})
      index = k;
      return
    end
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function [x, y, z] = normalizeStem3Data(x, y, z)
  if isvector(z)
    z = z(:);
  end
  if isvector(x) && isvector(y) && ~isvector(z)
    [x, y] = meshgrid(x(:).', y(:));
  end
  if isscalar(x)
    x = x * ones(size(z));
  end
  if isscalar(y)
    y = y * ones(size(z));
  end
  if isvector(x) && isvector(z) && numel(x) == numel(z)
    x = x(:);
  end
  if isvector(y) && isvector(z) && numel(y) == numel(z)
    y = y(:);
  end
  if ~isequal(size(x), size(y)) || ~isequal(size(x), size(z))
    error('Nelson:dimagree:xYZSizeX', 'X, Y, and Z must have the same size.');
  end
end
%=============================================================================
function group = drawStem3(ax, parent, x, y, z, filled, lineSpec, propertyPairs)
  [lineStyle, colorSpec, markerSpec, msg] = colstyle(lineSpec, 'plot');
  if ~isempty(msg)
    error('Nelson:stem3:invalidLineSpec', msg);
  end
  if isempty(lineStyle)
    lineStyle = '-';
  end
  if isempty(markerSpec)
    markerSpec = 'o';
  end
  if isempty(colorSpec)
    colorSpec = getColorAndUpdateIndex(ax);
  end
  x = x(:);
  y = y(:);
  z = z(:);
  if isempty(parent)
    objectParent = ax;
  else
    objectParent = parent;
  end
  markerFaceColor = 'none';
  if filled
    markerFaceColor = 'auto';
  end
  group = __stem__('Parent', objectParent, 'XData', x.', 'YData', y.', 'ZData', z.', ...
    'XDataMode', 'manual', 'YDataMode', 'manual', 'ZDataMode', 'manual', ...
    'Color', colorSpec, 'LineStyle', lineStyle, 'Marker', markerSpec, ...
    'MarkerEdgeColor', 'auto', 'MarkerFaceColor', markerFaceColor, propertyPairs{:});
  colorSpec = group.Color;
  lineStyle = group.LineStyle;
  markerSpec = group.Marker;
  markerFaceColor = resolvedStemColor(group.MarkerFaceColor, colorSpec);
  markerEdgeColor = resolvedStemColor(group.MarkerEdgeColor, colorSpec);
  baseValue = group.BaseValue;
  rendererParent = ax;
  renderer = hggroup(rendererParent);
  renderer.HandleVisibility = 'off';
  n = numel(z);
  stemX = reshape([x.'; x.'; NaN(1, n)], [], 1);
  stemY = reshape([y.'; y.'; NaN(1, n)], [], 1);
  stemZ = reshape([baseValue * ones(1, n); z.'; NaN(1, n)], [], 1);
  rendererPropertyPairs = stemRendererPropertyPairs(propertyPairs);
  plot3(renderer, stemX, stemY, stemZ, 'Color', colorSpec, 'LineStyle', lineStyle, ...
    'LineWidth', group.LineWidth, 'Marker', 'none', rendererPropertyPairs{:});
  markerProps = {'Color', colorSpec, 'LineStyle', 'none', 'Marker', markerSpec, ...
    'MarkerSize', group.MarkerSize, 'MarkerEdgeColor', markerEdgeColor, ...
    'MarkerFaceColor', markerFaceColor};
  plot3(renderer, x, y, z, markerProps{:}, rendererPropertyPairs{:});
  group.RendererState = struct('ChartType', 'stem3', 'Renderer', renderer);
  view(ax, 3);
  grid(ax, 'on');
end
%=============================================================================
function applyStem3ImplicitAxesState(ax, columnCount)
  if isempty(columnCount) || columnCount <= 1 || ~strcmp(ax.XLimMode, 'auto')
    return
  end
  ticks = ax.XTick;
  ax.XLim = [1, columnCount];
  if strcmp(ax.XTickMode, 'auto')
    ticks = ticks(ticks >= 1 & ticks <= columnCount);
    if isempty(ticks)
      ticks = 1:columnCount;
    end
    ax.XTick = ticks;
  end
end
%=============================================================================
function value = resolvedStemColor(value, color)
  if ischar(value) && strcmp(value, 'auto')
    value = color;
  end
end
%=============================================================================
function pairs = stemRendererPropertyPairs(propertyPairs)
  pairs = {};
  for k = 1:2:length(propertyPairs)
    propertyName = propertyPairs{k};
    if isTextScalar(propertyName)
      propertyName = convertStringsToChars(propertyName);
    end
    skipResolvedColor = strcmp(propertyName, 'MarkerFaceColor') || strcmp(propertyName, 'MarkerEdgeColor');
    if ~skipResolvedColor && isValidGraphicsProperty('line', propertyName)
      pairs = [pairs, propertyPairs(k:k + 1)];
    end
  end
end
%=============================================================================
