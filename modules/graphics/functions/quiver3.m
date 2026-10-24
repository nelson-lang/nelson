%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = quiver3(varargin)
  narginchk(4, 10000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseQuiver3Parent(args);
  [x, y, z, u, v, w, args, autoXY] = parseQuiver3Data(args);
  [scaleArgs, lineArgs, propertyPairs] = parseQuiver3Tail(args);
  ax = targetAxes(parent);
  constructorArgs = {'Parent', parentForObject(parent, ax), 'XData', x, 'YData', y, ...
    'ZData', z, 'UData', u, 'VData', v, 'WData', w, ...
    'XDataMode', autoMode(autoXY), 'YDataMode', autoMode(autoXY)};
  constructorArgs = [constructorArgs, scaleArgs, lineArgs];
  if isempty(lineColor(lineArgs)) && ~hasProperty(propertyPairs, 'Color')
    constructorArgs = [constructorArgs, {'Color', getColorAndUpdateIndex(ax)}];
  end
  h = __quiver__(constructorArgs{:}, propertyPairs{:});
  view(ax, 3);
  grid(ax, 'on');
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseQuiver3Parent(args)
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  else
    parent = [];
  end
end
%=============================================================================
function [x, y, z, u, v, w, args, autoXY] = parseQuiver3Data(args)
  firstTail = firstTailArgument(args);
  if firstTail >= 7
    x = args{1};
    y = args{2};
    z = args{3};
    u = args{4};
    v = args{5};
    w = args{6};
    args = args(7:end);
    autoXY = false;
  elseif firstTail >= 5
    z = args{1};
    u = args{2};
    v = args{3};
    w = args{4};
    [x, y] = defaultGrid(u);
    args = args(5:end);
    autoXY = true;
  else
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  validateQuiver3Data(x, y, z, u, v, w);
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
function validateQuiver3Data(x, y, z, u, v, w)
  values = {x, y, z, u, v, w};
  for k = 1:numel(values)
    if ~((isnumeric(values{k}) || islogical(values{k})) && isreal(values{k}))
      error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
    end
  end
  if ~isequal(size(u), size(v)) || ~isequal(size(u), size(w))
    error('Nelson:dimagree:vectorComponentSizesMatch', 'Vector component sizes must match.');
  end
  validateCoordinateSize(x, size(u), 'X');
  validateCoordinateSize(y, size(u), 'Y');
  validateCoordinateSize(z, size(u), 'Z');
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
function [scaleArgs, lineArgs, propertyPairs] = parseQuiver3Tail(args)
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
    error('Nelson:quiver3:invalidLineSpec', msg);
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
