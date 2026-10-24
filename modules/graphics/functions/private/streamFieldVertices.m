%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [vertices, field] = streamFieldVertices(dataArgs)
  % Traces a 2-D or 3-D vector field from the start points it was given and
  % returns one N-by-2 or N-by-3 array of vertices per start point. This is the
  % computation behind streamline, stream2 and stream3: sharing it is what keeps
  % streamline(stream2(X, Y, U, V, sx, sy)) on the same path as
  % streamline(X, Y, U, V, sx, sy). No graphics object is created here, so a
  % caller that only wants the vertices does not open a figure.
  field = parseStreamField(dataArgs);
  if isfield(field, 'Vertices')
    vertices = field.Vertices(:);
    return
  end
  if field.Dimension == 2
    starts = [field.sx(:), field.sy(:)];
  else
    starts = [field.sx(:), field.sy(:), field.sz(:)];
  end
  vertices = cell(size(starts, 1), 1);
  for k = 1:size(starts, 1)
    vertices{k} = traceStream(field, starts(k, :));
  end
end
%=============================================================================
function field = parseStreamField(dataArgs)
  switch numel(dataArgs)
    case 1
      if ~iscell(dataArgs{1})
        error('Nelson:graphics:invalidStreamlineVertices', '%s must be a cell array.', 'Stream vertices');
      end
      field.Vertices = dataArgs{1};
      field.Dimension = streamVerticesDimension(field.Vertices);
    case 4
      [field.x, field.y] = default2DGrid(dataArgs{1});
      field.u = double(dataArgs{1});
      field.v = double(dataArgs{2});
      field.sx = dataArgs{3};
      field.sy = dataArgs{4};
      field.sz = zeros(size(field.sx));
      field.Dimension = 2;
    case 6
      [field, ok] = parseSixStreamArgs(dataArgs);
      if ~ok
        error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
      end
    case 5
      field = parseStreamField(dataArgs(1:4));
      field.Options = dataArgs{5};
    case 7
      field = parseStreamField(dataArgs(1:6));
      field.Options = dataArgs{7};
    case 9
      field.x = dataArgs{1}; field.y = dataArgs{2}; field.z = dataArgs{3};
      field.u = double(dataArgs{4}); field.v = double(dataArgs{5}); field.w = double(dataArgs{6});
      field.sx = dataArgs{7}; field.sy = dataArgs{8}; field.sz = dataArgs{9};
      field.Dimension = 3;
    case 10
      field = parseStreamField(dataArgs(1:9));
      field.Options = dataArgs{10};
    otherwise
      if numel(dataArgs) < 1
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end
      if numel(dataArgs) > 10
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  if ~isfield(field, 'Vertices')
    field = normalizeStreamField(field);
  end
end
%=============================================================================
function dimension = streamVerticesDimension(vertices)
  dimension = 2;
  for k = 1:numel(vertices)
    if ~isempty(vertices{k}) && size(vertices{k}, 2) == 3
      dimension = 3;
      return
    end
  end
end
%=============================================================================
function [field, ok] = parseSixStreamArgs(args)
  ok = true;
  if ndims(args{1}) == 3
    [field.x, field.y, field.z] = volumeGridVectors(args{1});
    field.u = double(args{1}); field.v = double(args{2}); field.w = double(args{3});
    field.sx = args{4}; field.sy = args{5}; field.sz = args{6};
    field.Dimension = 3;
  else
    field.x = args{1}; field.y = args{2};
    field.u = double(args{3}); field.v = double(args{4});
    field.sx = args{5}; field.sy = args{6}; field.sz = zeros(size(field.sx));
    field.Dimension = 2;
  end
end
%=============================================================================
function field = normalizeStreamField(field)
  if field.Dimension == 2
    [field.x, field.y] = gridVectors2D(field.x, field.y, field.u);
    if ~isequal(size(field.u), size(field.v))
      error('Nelson:dimagree:vectorComponentSizesMatch', 'Vector component sizes must match.');
    end
  else
    [field.x, field.y, field.z] = volumeGridVectors(field.x, field.y, field.z, field.u);
    if ~isequal(size(field.u), size(field.v)) || ~isequal(size(field.u), size(field.w))
      error('Nelson:dimagree:vectorComponentSizesMatch', 'Vector component sizes must match.');
    end
  end
  if numel(field.sx) ~= numel(field.sy) || numel(field.sx) ~= numel(field.sz)
    error('Nelson:streamlineBase:invalidStartPointArrays', 'Start point arrays must have the same number of elements.');
  end
end
%=============================================================================
function [x, y] = default2DGrid(U)
  x = 1:size(U, 2);
  y = 1:size(U, 1);
end
%=============================================================================
function [x, y] = gridVectors2D(X, Y, U)
  if isvector(X) && isvector(Y)
    x = double(X(:).');
    y = double(Y(:).');
  elseif isequal(size(X), size(Y)) && isequal(size(X), size(U))
    x = double(X(1, :));
    y = double(Y(:, 1).');
  else
    error('Nelson:streamlineBase:invalidGridArrays', 'Grid arrays must have the same size as vector components.');
  end
end
%=============================================================================
function pts = traceStream(field, start)
  if outsideField(field, start)
    % A start point outside the field traces nothing at all. A start inside it
    % that the field does not move keeps its single vertex.
    pts = [];
    return
  end
  pts = start;
  [step, maxVertices] = streamIntegrationOptions(field);
  current = start;
  for k = 1:(maxVertices - 1)
    vec = sampleVector(field, current);
    if any(~isfinite(vec)) || norm(vec) < eps
      break
    end
    vec = vec / norm(vec);
    current = current + step * vec;
    if outsideField(field, current)
      break
    end
    pts(end + 1, :) = current;
  end
end
%=============================================================================
function [step, maxVertices] = streamIntegrationOptions(field)
  % The options argument is [stepsize] or [stepsize, maxvert]: a step given in
  % grid cells, and the largest number of vertices to produce, the start point
  % counted. Both keep their default when the argument leaves them out.
  stepScale = 0.1;
  maxVertices = 10000;
  if isfield(field, 'Options') && ~isempty(field.Options) && isnumeric(field.Options)
    options = double(field.Options(:).');
    if isfinite(options(1)) && options(1) > 0
      stepScale = options(1);
    end
    if numel(options) >= 2 && isfinite(options(2)) && options(2) >= 1
      maxVertices = floor(options(2));
    end
  end
  step = stepScale * streamGridSpacing(field);
end
%=============================================================================
function spacing = streamGridSpacing(field)
  % One grid cell, taken as the smallest gap between two distinct coordinates.
  values = [field.x(:); field.y(:)];
  if field.Dimension == 3
    values = [values; field.z(:)];
  end
  d = diff(unique(sort(values)));
  d = d(d > 0);
  if isempty(d)
    spacing = 1;
  else
    spacing = min(d);
  end
end
%=============================================================================
function vec = sampleVector(field, point)
  if field.Dimension == 2
    ux = interp2(field.x, field.y, field.u, point(1), point(2), 'linear', NaN);
    vy = interp2(field.x, field.y, field.v, point(1), point(2), 'linear', NaN);
    vec = [ux, vy];
  else
    ux = interp3(field.x, field.y, field.z, field.u, point(1), point(2), point(3), 'linear', NaN);
    vy = interp3(field.x, field.y, field.z, field.v, point(1), point(2), point(3), 'linear', NaN);
    wz = interp3(field.x, field.y, field.z, field.w, point(1), point(2), point(3), 'linear', NaN);
    vec = [ux, vy, wz];
  end
end
%=============================================================================
function tf = outsideField(field, point)
  tf = point(1) < min(field.x) || point(1) > max(field.x) ...
    || point(2) < min(field.y) || point(2) > max(field.y);
  if field.Dimension == 3
    tf = tf || point(3) < min(field.z) || point(3) > max(field.z);
  end
end
%=============================================================================
