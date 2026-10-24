%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = streamribbon(varargin)
  narginchk(2, 10);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, ax, args] = streamRibbonTarget(args);
  [vertices, twistangle, width] = parseStreamRibbonInputs(parent, args);
  if isempty(width)
    width = automaticRibbonWidth(vertices);
  end
  h = ribbonSurfaces(parent, vertices, twistangle, width);
  applyRibbonAxesState(ax, h, vertices);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, ax, args] = streamRibbonTarget(args)
  [parent, args] = graphicsParseParent(args);
  ax = graphicsAddTargetAxes(parent);
  if isempty(parent)
    parent = ax;
  end
end
%=============================================================================
function applyRibbonAxesState(ax, h, vertices)
  if isempty(ax) || isempty(h) || ~strcmp(ax.XLimMode, 'auto')
    return
  end
  xData = [];
  zData = [];
  for k = 1:numel(h)
    if isgraphics(h(k), 'surface')
      xData = [xData; h(k).XData(:)];
      zData = [zData; h(k).ZData(:)];
    end
  end
  limits = niceRibbonLimits(xData);
  if isempty(limits)
    return
  end
  ax.XLim = limits;
  step = niceRibbonStep(diff(limits) / 8);
  ax.XTick = limits(1):step:limits(2);
  zLimits = tightRibbonZLimits(zData, vertices);
  if ~isempty(zLimits)
    ax.ZLim = zLimits;
  end
end
%=============================================================================
function limits = tightRibbonZLimits(values, vertices)
  values = values(isfinite(values));
  if isempty(values)
    limits = [];
    return
  end
  limits = [min(values), maxStreamVertexZ(vertices)];
  if limits(1) == limits(2)
    limits = [limits(1) - 1, limits(2) + 1];
  end
end
%=============================================================================
function upper = maxStreamVertexZ(vertices)
  upper = -Inf;
  for k = 1:numel(vertices)
    pts = vertices{k};
    if ~isempty(pts)
      upper = max(upper, max(pts(:, 3)));
    end
  end
  if ~isfinite(upper)
    upper = 0;
  end
  step = niceRibbonStep(abs(upper) / 8);
  if step > 0
    upper = ceil(upper / step) * step;
  end
end
%=============================================================================
function limits = niceRibbonLimits(values)
  values = values(isfinite(values));
  if isempty(values)
    limits = [];
    return
  end
  lower = min(values);
  upper = max(values);
  if lower == upper
    limits = [lower - 1, upper + 1];
    return
  end
  step = niceRibbonStep((upper - lower) / 8);
  limits = [floor(lower / step) * step, ceil(upper / step) * step];
  limits(abs(limits) < step * 1e-10) = 0;
end
%=============================================================================
function step = niceRibbonStep(rawStep)
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
end
%=============================================================================
function [vertices, twistangle, width] = parseStreamRibbonInputs(parent, args)
  width = [];
  if iscell(args{1})
    [vertices, twistangle, width] = parseVertexRibbonInputs(args);
    return
  end
  switch numel(args)
    case {6, 7}
      fieldArgs = args(1:6);
      if numel(args) == 7
        width = args{7};
      end
    case {9, 10}
      fieldArgs = args(1:9);
      if numel(args) == 10
        width = args{10};
      end
    otherwise
      if numel(args) < 6
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end
      if numel(args) > 10
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  [vertices, twistangle] = streamVerticesFromField(parent, fieldArgs);
  width = validateRibbonWidth(width);
end
%=============================================================================
function [vertices, twistangle, width] = parseVertexRibbonInputs(args)
  switch numel(args)
    case {2, 3}
      vertices = validateRibbonVertices(args{1});
      twistangle = validateTwistAngles(args{2}, vertices);
      if numel(args) == 3
        width = validateRibbonWidth(args{3});
      else
        width = [];
      end
    otherwise
      if numel(args) < 2
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end
      error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
end
%=============================================================================
function [vertices, twistangle] = streamVerticesFromField(parent, args)
  % The stream lines are traced in the target itself and thrown away right
  % after: tracing them in the current axes stretched the limits of an axes
  % this call was never handed.
  [lines, state] = streamlineBase(parent, args{:});
  vertices = cell(numel(lines), 1);
  twistangle = cell(numel(lines), 1);
  for k = 1:numel(lines)
    vertices{k} = [lines(k).XData(:), lines(k).YData(:), lines(k).ZData(:)];
    twistangle{k} = zeros(size(vertices{k}, 1), 1);
  end
  delete(lines);
end
%=============================================================================
function vertices = validateRibbonVertices(vertices)
  if ~iscell(vertices)
    error('Nelson:graphics:invalidStreamlineVertices', '%s must be a cell array.', 'Streamline vertices');
  end
  for k = 1:numel(vertices)
    pts = vertices{k};
    if ~isnumeric(pts) || size(pts, 2) ~= 3
      error('Nelson:graphics:verticesNotNumericArrays', 'Streamline vertices must be numeric arrays with three columns.');
    end
    vertices{k} = double(pts);
  end
end
%=============================================================================
function twistangle = validateTwistAngles(twistangle, vertices)
  if ~iscell(twistangle) || numel(twistangle) ~= numel(vertices)
    error('Nelson:streamribbon:invalidTwistAngles', 'Twist angles must be a cell array matching streamline vertices.');
  end
  for k = 1:numel(twistangle)
    values = twistangle{k};
    if ~isnumeric(values) || numel(values) ~= size(vertices{k}, 1)
      error('Nelson:streamribbon:twistAnglesCountMismatch', 'Twist angles must match streamline vertex counts.');
    end
    twistangle{k} = double(values(:));
  end
end
%=============================================================================
function width = validateRibbonWidth(width)
  if isempty(width)
    return
  end
  if ~isnumeric(width) || ~isscalar(width) || ~isfinite(width) || width <= 0
    error('Nelson:graphics:mustBePositiveScalar', '%s must be a positive scalar.', 'Ribbon width');
  end
  width = double(width);
end
%=============================================================================
function width = automaticRibbonWidth(vertices)
  bounds = zeros(0, 6);
  for k = 1:numel(vertices)
    pts = vertices{k};
    if ~isempty(pts)
      bounds(end + 1, :) = [min(pts, [], 1), max(pts, [], 1)];
    end
  end
  if isempty(bounds)
    width = 0.05;
    return
  end
  span = max(bounds(:, 4:6), [], 1) - min(bounds(:, 1:3), [], 1);
  width = 0.05 * max(span);
end
%=============================================================================
function h = ribbonSurfaces(parent, vertices, twistangle, width)
  h = gobjects(0, 1);
  for k = 1:numel(vertices)
    pts = vertices{k};
    if size(pts, 1) > 1
      h(end + 1, 1) = ribbonSurface(parent, pts, twistangle{k}, width);
    end
  end
end
%=============================================================================
function h = ribbonSurface(parent, pts, twistangle, width)
  [X, Y, Z, C] = ribbonCoordinates(pts, twistangle, width);
  h = surface(parent, X, Y, Z, C, 'FaceColor', 'flat', ...
    'EdgeColor', graphicsDefaultEdgeColor());
end
%=============================================================================
function [X, Y, Z, C] = ribbonCoordinates(vertices, twistangle, width)
  radius = width / 2;
  count = size(vertices, 1);
  side = initialRibbonSide(vertices);
  side = side * radius;
  side = rotateGeometry(side, vertices(2, :) - vertices(1, :), twistangle(1));

  X = zeros(count, 2);
  Y = zeros(count, 2);
  Z = zeros(count, 2);
  leftEdge = vertices(1, :) + side;
  rightEdge = vertices(1, :) - side;
  X(1, :) = [leftEdge(1), rightEdge(1)];
  Y(1, :) = [leftEdge(2), rightEdge(2)];
  Z(1, :) = [leftEdge(3), rightEdge(3)];

  for k = 2:count
    direction = vertices(k, :) - vertices(k - 1, :);
    side = nextRibbonSide(side, direction, twistangle(k), radius);
    leftEdge = vertices(k, :) + side;
    rightEdge = vertices(k, :) - side;
    X(k, :) = [leftEdge(1), rightEdge(1)];
    Y(k, :) = [leftEdge(2), rightEdge(2)];
    Z(k, :) = [leftEdge(3), rightEdge(3)];
  end
  twist = cumsum(twistangle(:));
  C = [twist, twist];
end
%=============================================================================
function side = nextRibbonSide(side, direction, angle, radius)
  side = rotateGeometry(side, direction, angle);
  side = crossSimple(crossSimple(direction, side), direction);
  sideLength = norm(side);
  if sideLength > 0
    side = side .* (1 / sideLength);
  end
  side = side * radius;
end
%=============================================================================
function side = initialRibbonSide(vertices)
  direction = vertices(2, :) - vertices(1, :);
  axes = [0 0 1; 1 0 0];
  normal = crossSimple(direction, axes(1, :));
  if ~any(normal)
    normal = crossSimple(direction, axes(2, :));
  end
  side = crossSimple(normal, direction);
  sideLength = norm(side);
  if sideLength > 0
    side = side .* (1 / sideLength);
  end
end
%=============================================================================
function c = crossSimple(a, b)
  c1 = a(2) * b(3) - a(3) * b(2);
  c2 = a(3) * b(1) - a(1) * b(3);
  c3 = a(1) * b(2) - a(2) * b(1);
  c = [c1, c2, c3];
end
%=============================================================================
function out = rotateGeometry(v, axisVector, angle)
  axisNorm = norm(axisVector);
  if axisNorm == 0 || angle == 0
    out = v;
    return
  end
  u = axisVector(:) / axisNorm;
  cosa = cos(angle);
  sina = sin(angle);
  vera = 1 - cosa;
  x = u(1);
  y = u(2);
  z = u(3);
  rotation = [cosa + x ^ 2 * vera, x * y * vera - z * sina, x * z * vera + y * sina; ...
    x * y * vera + z * sina, cosa + y ^ 2 * vera, y * z * vera - x * sina; ...
    x * z * vera - y * sina, y * z * vera + x * sina, cosa + z ^ 2 * vera].';
  out = v * rotation;
end
%=============================================================================
