%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = quiver(varargin)
  narginchk(2, 10000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseQuiverParent(args);
  [x, y, u, v, args, autoXY] = parseQuiverData(args);
  [args, filled] = extractQuiverFilledFlag(args);
  [scaleArgs, lineArgs, propertyPairs] = parseQuiverTail(args);
  keepBox = quiverKeepBox(parent);
  ax = targetAxes(parent);
  if ~keepBox
    ax.Box = 'on';
  end
  constructorArgs = {'Parent', parentForObject(parent, ax), 'XData', x, 'YData', y, ...
    'UData', u, 'VData', v, 'XDataMode', autoMode(autoXY), 'YDataMode', autoMode(autoXY)};
  constructorArgs = [constructorArgs, scaleArgs, lineArgs];
  if filled && ~hasProperty(propertyPairs, 'MarkerFaceColor')
    constructorArgs = [constructorArgs, {'MarkerFaceColor', 'auto'}];
  end
  if isempty(lineColor(lineArgs)) && ~hasProperty(propertyPairs, 'Color')
    color = getColorAndUpdateIndex(ax);
    constructorArgs = [constructorArgs, {'Color', color}];
  end
  h = __quiver__(constructorArgs{:}, propertyPairs{:});
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseQuiverParent(args)
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  else
    [parent, args] = graphicsExtractParentProperty(args);
  end
end
%=============================================================================
function keepBox = quiverKeepBox(parent)
  keepBox = false;
  if isempty(parent)
    fig = groot().CurrentFigure;
    if ~isempty(fig) && ~isempty(fig.CurrentAxes)
      keepBox = strcmp(fig.CurrentAxes.NextPlot, 'add');
    end
  elseif isgraphics(parent, 'axes')
    keepBox = strcmp(parent.NextPlot, 'add');
  elseif isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
    keepBox = strcmp(ax.NextPlot, 'add');
  end
end
%=============================================================================
function [x, y, u, v, args, autoXY] = parseQuiverData(args)
  firstTail = firstTailArgument(args);
  if firstTail >= 5
    x = args{1};
    y = args{2};
    u = args{3};
    v = args{4};
    args = args(5:end);
    autoXY = false;
  elseif firstTail >= 3
    u = args{1};
    v = args{2};
    [x, y] = defaultGrid(u);
    args = args(3:end);
    autoXY = true;
  else
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  validateQuiverData(x, y, u, v);
end
%=============================================================================
function index = firstTailArgument(args)
  index = length(args) + 1;
  for k = 1:length(args)
    if ~(isnumeric(args{k}) || islogical(args{k}))
      index = k;
      return
    end
  end
end
%=============================================================================
function [x, y] = defaultGrid(u)
  x = 1:size(u, 2);
  y = 1:size(u, 1);
end
%=============================================================================
function validateQuiverData(x, y, u, v)
  values = {x, y, u, v};
  for k = 1:numel(values)
    if ~((isnumeric(values{k}) || islogical(values{k})) && isreal(values{k}))
      error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
    end
  end
  if ~isequal(size(u), size(v))
    error('Nelson:dimagree:vectorComponentSizesMatch', 'Vector component sizes must match.');
  end
  validateCoordinateSize(x, size(u), 'X');
  validateCoordinateSize(y, size(u), 'Y');
end
%=============================================================================
function validateCoordinateSize(value, targetSize, name)
  if isscalar(value) || isequal(size(value), targetSize)
    return
  end
  if isvector(value) && (numel(value) == targetSize(1) || numel(value) == targetSize(2) || numel(value) == prod(targetSize))
    return
  end
  error('Nelson:graphics:componentDataSizeMismatch', '%s must match vector component size.', name);
end
%=============================================================================
function [scaleArgs, lineArgs, propertyPairs] = parseQuiverTail(args)
  scaleArgs = {};
  if ~isempty(args) && isnumeric(args{1}) && isscalar(args{1})
    scale = double(args{1});
    if scale == 0
      scaleArgs = {'AutoScale', 'off'};
    else
      scaleArgs = {'AutoScale', 'on', 'AutoScaleFactor', scale};
    end
    args = args(2:end);
  end
  lineArgs = parseLineSpec(args);
  if ~isempty(lineArgs)
    args = args(2:end);
  end
  if mod(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  propertyPairs = args;
end
%=============================================================================
function lineArgs = parseLineSpec(args)
  lineArgs = {};
  if isempty(args) || ~isTextScalar(args{1}) || isValidGraphicsProperty('quiver', args{1})
    return
  end
  [lineStyle, colorSpec, markerSpec, msg] = colstyle(convertStringsToChars(args{1}), 'plot');
  if ~isempty(msg)
    error('Nelson:quiver:invalidLineSpec', msg);
  end
  if ~isempty(lineStyle)
    lineArgs = [lineArgs, {'LineStyle', lineStyle}];
  end
  if ~isempty(colorSpec)
    lineArgs = [lineArgs, {'Color', colorSpec}];
  end
  if ~isempty(markerSpec)
    lineArgs = [lineArgs, {'Marker', markerSpec}];
  end
end
%=============================================================================
function [args, filled] = extractQuiverFilledFlag(args)
  % 'filled' is a standalone flag (fill the arrow markers), not a name-value
  % pair. Remove it from the tail and signal it so the caller can map it to
  % MarkerFaceColor; no quiver property takes 'filled' as its value, so this is
  % unambiguous.
  filled = false;
  keep = true(1, numel(args));
  for k = 1:numel(args)
    if isTextScalar(args{k}) && strcmpi(char(args{k}), 'filled')
      filled = true;
      keep(k) = false;
    end
  end
  args = args(keep);
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function value = autoMode(tf)
  if tf
    value = 'auto';
  else
    value = 'manual';
  end
end
%=============================================================================
function ax = targetAxes(parent)
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = newplot(parent);
  end
end
%=============================================================================
function parent = parentForObject(parent, ax)
  if isempty(parent)
    parent = ax;
  end
end
%=============================================================================
function tf = hasProperty(args, name)
  tf = false;
  for k = 1:2:length(args)
    if isTextScalar(args{k}) && strcmpi(convertStringsToChars(args{k}), name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function value = lineColor(args)
  value = [];
  for k = 1:2:length(args)
    if strcmp(args{k}, 'Color')
      value = args{k + 1};
      return
    end
  end
end
%=============================================================================
