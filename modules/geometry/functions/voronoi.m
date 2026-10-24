%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [a, b] = voronoi(varargin)
  nargoutchk(0, 2);
  narginchk(1, 4);
  points = parseVoronoiInputs(varargin{:});
  if size(points, 2) ~= 2
    error('Nelson:geometry:twoDimensionalPointsExpected', 'Two-dimensional points expected.');
  end
  [vertices, cells] = voronoin(points);
  [vx, vy] = voronoiSegments(vertices, cells, points);
  if nargout == 0
    plot(points(:, 1), points(:, 2), '.', vx, vy);
    return
  end
  if nargout == 1
    a = plot(points(:, 1), points(:, 2), '.', vx, vy);
  else
    a = vx;
    b = vy;
  end
end
%=============================================================================
function points = parseVoronoiInputs(varargin)
  if length(varargin) == 1 && isa(varargin{1}, 'delaunayTriangulation')
    points = varargin{1}.Points;
  elseif length(varargin) == 1
    points = varargin{1};
  elseif length(varargin) == 2
    points = [varargin{1}(:), varargin{2}(:)];
  elseif length(varargin) == 3
    points = [varargin{1}(:), varargin{2}(:)];
  else
    error('Nelson:geometry:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function [vx, vy] = voronoiSegments(vertices, cells, points)
  vx = [];
  vy = [];
  for k = 1:length(cells)
    cellIds = cells{k};
    cellIds = cellIds(isfinite(cellIds) & cellIds > 1);
    if length(cellIds) < 2
      continue
    end
    cellIds(end + 1) = cellIds(1);
    x = vertices(cellIds, 1);
    y = vertices(cellIds, 2);
    vx = [vx; [x(1:end - 1), x(2:end), NaN(size(x(1:end - 1)))](:)];
    vy = [vy; [y(1:end - 1), y(2:end), NaN(size(y(1:end - 1)))](:)];
  end
  if isempty(vx)
    [vx, vy] = boundedBisectors(points);
  end
end
%=============================================================================
function [vx, vy] = boundedBisectors(points)
  tri = delaunay(points);
  edgeList = unique(sort([tri(:, [1 2]); tri(:, [1 3]); tri(:, [2 3])], 2), 'rows');
  minp = min(points, [], 1);
  maxp = max(points, [], 1);
  span = max(maxp - minp);
  if span == 0
    span = 1;
  end
  minp = minp - span;
  maxp = maxp + span;
  vx = [];
  vy = [];
  for k = 1:size(edgeList, 1)
    a = points(edgeList(k, 1), :);
    b = points(edgeList(k, 2), :);
    mid = (a + b) ./ 2;
    direction = [-(b(2) - a(2)), b(1) - a(1)];
    normDirection = sqrt(sum(direction .* direction));
    if normDirection == 0
      continue
    end
    direction = direction ./ normDirection;
    segment = clipLineToBox(mid, direction, minp, maxp);
    if ~isempty(segment)
      vx = [vx; segment(1, 1); segment(2, 1); NaN];
      vy = [vy; segment(1, 2); segment(2, 2); NaN];
    end
  end
end
%=============================================================================
function segment = clipLineToBox(point, direction, minp, maxp)
  candidates = [];
  if direction(1) ~= 0
    for x = [minp(1), maxp(1)]
      t = (x - point(1)) / direction(1);
      y = point(2) + t * direction(2);
      if y >= minp(2) && y <= maxp(2)
        candidates = [candidates; x, y];
      end
    end
  end
  if direction(2) ~= 0
    for y = [minp(2), maxp(2)]
      t = (y - point(2)) / direction(2);
      x = point(1) + t * direction(1);
      if x >= minp(1) && x <= maxp(1)
        candidates = [candidates; x, y];
      end
    end
  end
  candidates = unique(candidates, 'rows');
  if size(candidates, 1) < 2
    segment = [];
  else
    segment = candidates(1:2, :);
  end
end
%=============================================================================
