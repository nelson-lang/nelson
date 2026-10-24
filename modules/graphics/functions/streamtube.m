%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = streamtube(varargin)
  narginchk(1, 10);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = streamTubeTarget(args);
  [vertices, width, scale, sides] = parseStreamTubeInputs(parent, args);
  if isempty(width)
    width = automaticTubeWidth(vertices) * scale;
  elseif iscell(width)
    for k = 1:numel(width)
      width{k} = width{k} * scale;
    end
  else
    width = width * scale;
  end
  h = tubeSurfaces(parent, vertices, width, sides);
  applyTubeAxesState(parent, h, vertices);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = streamTubeTarget(args)
  [parent, args] = graphicsParseParent(args);
  if isempty(parent)
    parent = graphicsAddTargetAxes(parent);
  end
end
%=============================================================================
function applyTubeAxesState(parent, h, vertices)
  if isempty(parent) || isempty(h) || ~strcmp(parent.ZLimMode, 'auto')
    return
  end
  zData = [];
  for k = 1:numel(h)
    if isgraphics(h(k), 'surface')
      zData = [zData; h(k).ZData(:)];
    end
  end
  zLimits = tightTubeZLimits(zData, vertices);
  if ~isempty(zLimits)
    parent.ZLim = zLimits;
  end
end
%=============================================================================
function limits = tightTubeZLimits(values, vertices)
  values = values(isfinite(values));
  if isempty(values)
    limits = [];
    return
  end
  limits = [min(values), maxTubeVertexZ(vertices)];
  if limits(1) == limits(2)
    limits = [limits(1) - 1, limits(2) + 1];
  end
end
%=============================================================================
function upper = maxTubeVertexZ(vertices)
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
  step = niceTubeZStep(abs(upper) / 8);
  if step > 0
    upper = ceil(upper / step) * step;
  end
end
%=============================================================================
function step = niceTubeZStep(rawStep)
  if rawStep <= 0 || ~isfinite(rawStep)
    step = 1;
    return
  end
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
function [vertices, width, scale, sides] = parseStreamTubeInputs(parent, args)
  width = [];
  scale = 1;
  sides = 20;
  if iscell(args{1})
    vertices = validateTubeVertices(args{1});
    switch numel(args)
      case 1
        return
      case 2
        value = args{2};
        if isnumeric(value) && isscalar(value)
          width = validateTubeWidth(value, vertices);
        elseif isnumeric(value) && numel(value) == 2
          scale = double(value(1));
          sides = double(value(2));
        else
          width = validateTubeWidth(value, vertices);
        end
      case 3
        width = validateTubeWidth(args{2}, vertices);
        [scale, sides] = parseScaleAndSides(args{3});
      otherwise
        error('Nelson:tooManyInputs', 'Too many input arguments.');
    end
    sides = validateTubeSides(sides);
    return
  end
  switch numel(args)
    case {6, 7}
      fieldArgs = args(1:6);
      if numel(args) == 7
        [scale, sides] = parseScaleAndSides(args{7});
      end
    case {9, 10}
      fieldArgs = args(1:9);
      if numel(args) == 10
        [scale, sides] = parseScaleAndSides(args{10});
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
  vertices = streamTubeVerticesFromField(parent, fieldArgs);
  sides = validateTubeSides(sides);
end
%=============================================================================
function vertices = streamTubeVerticesFromField(parent, args)
  % The stream lines are traced in the target itself and thrown away right
  % after: tracing them in the current axes stretched the limits of an axes
  % this call was never handed.
  [lines, state] = streamlineBase(parent, args{:});
  vertices = cell(numel(lines), 1);
  for k = 1:numel(lines)
    vertices{k} = [lines(k).XData(:), lines(k).YData(:), lines(k).ZData(:)];
  end
  delete(lines);
end
%=============================================================================
function vertices = validateTubeVertices(vertices)
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
function width = validateTubeWidth(width, vertices)
  if iscell(width)
    if numel(width) ~= numel(vertices)
      error('Nelson:streamtube:tubeWidthsVerticesMismatch', 'Tube widths must match streamline vertices.');
    end
    for k = 1:numel(width)
      values = width{k};
      if ~isnumeric(values) || ~(isscalar(values) || numel(values) == size(vertices{k}, 1))
        error('Nelson:streamtube:invalidTubeWidths', 'Tube widths must be scalar or match streamline vertex counts.');
      end
      width{k} = double(values(:));
    end
    return
  end
  if ~isnumeric(width) || ~isscalar(width) || ~isfinite(width) || width <= 0
    error('Nelson:graphics:mustBePositiveScalar', '%s must be a positive scalar.', 'Tube width');
  end
  width = double(width);
end
%=============================================================================
function [scale, sides] = parseScaleAndSides(value)
  if isempty(value)
    scale = 1;
    sides = 20;
    return
  end
  if ~isnumeric(value) || numel(value) ~= 2
    error('Nelson:streamtube:invalidScaleInput', 'Scale input must be a two-element numeric vector.');
  end
  scale = double(value(1));
  sides = double(value(2));
end
%=============================================================================
function sides = validateTubeSides(sides)
  if ~isscalar(sides) || ~isfinite(sides) || sides < 3
    error('Nelson:streamtube:invalidTubeSideCount', 'Tube side count must be at least three.');
  end
  sides = floor(sides);
end
%=============================================================================
function width = automaticTubeWidth(vertices)
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
function h = tubeSurfaces(parent, vertices, width, sides)
  h = gobjects(0, 1);
  for k = 1:numel(vertices)
    pts = removeRepeatedVertices(vertices{k});
    if size(pts, 1) > 1
      if iscell(width)
        localWidth = width{k};
      else
        localWidth = width;
      end
      [X, Y, Z] = tubeCoordinates(pts, localWidth, sides);
      h(end + 1, 1) = surface(parent, X, Y, Z, 'FaceColor', 'flat', ...
        'EdgeColor', graphicsDefaultEdgeColor());
    end
  end
end
%=============================================================================
function vertices = removeRepeatedVertices(vertices)
  if size(vertices, 1) < 2
    return
  end
  keep = [true; any(diff(vertices), 2)];
  vertices = vertices(keep, :);
end
%=============================================================================
function [X, Y, Z] = tubeCoordinates(vertices, width, sides)
  count = size(vertices, 1);
  if isscalar(width)
    width = repmat(width, count, 1);
  else
    width = width(:);
    if numel(width) ~= count
      width = interp1(linspace(0, 1, numel(width)), width, linspace(0, 1, count), 'linear').';
    end
  end
  radius = width(:).' / 2;
  normals = tubeNormals(vertices);
  tangents = diff(vertices);
  tangents(end + 1, :) = tangents(end, :);
  binormals = crossRows(tangents, normals);
  speed = sqrt(sum(tangents .^ 2, 2));
  speed(speed == 0) = 1;
  binormals = binormals ./ speed;
  angles = (0:sides)' .* (2 * pi / sides);
  cosAngles = cos(angles);
  sinAngles = sin(angles);
  X = tubeSurfaceCoordinate(vertices(:, 1).', normals(:, 1).', binormals(:, 1).', radius, cosAngles, sinAngles);
  Y = tubeSurfaceCoordinate(vertices(:, 2).', normals(:, 2).', binormals(:, 2).', radius, cosAngles, sinAngles);
  Z = tubeSurfaceCoordinate(vertices(:, 3).', normals(:, 3).', binormals(:, 3).', radius, cosAngles, sinAngles);
end
%=============================================================================
function coordinate = tubeSurfaceCoordinate(origin, normal, binormal, radius, cosAngles, sinAngles)
  base = ones(size(cosAngles)) * origin;
  offset = cosAngles * normal + sinAngles * binormal;
  coordinate = (base + offset .* radius).';
end
%=============================================================================
function normals = tubeNormals(vertices)
  count = size(vertices, 1);
  normals = zeros(count, 3);
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
  normals(1, :) = side;
  for k = 1:count - 1
    direction = vertices(k + 1, :) - vertices(k, :);
    normal = crossSimple(direction, side);
    side = crossSimple(normal, direction);
    sideLength = norm(side);
    if sideLength > 0
      side = side .* (1 / sideLength);
    end
    normals(k + 1, :) = side;
  end
end
%=============================================================================
function c = crossRows(a, b)
  c = [a(:, 2) .* b(:, 3) - a(:, 3) .* b(:, 2), ...
    a(:, 3) .* b(:, 1) - a(:, 1) .* b(:, 3), ...
    a(:, 1) .* b(:, 2) - a(:, 2) .* b(:, 1)];
end
%=============================================================================
function c = crossSimple(a, b)
  c1 = a(2) * b(3) - a(3) * b(2);
  c2 = a(3) * b(1) - a(1) * b(3);
  c3 = a(1) * b(2) - a(2) * b(1);
  c = [c1, c2, c3];
end
%=============================================================================
