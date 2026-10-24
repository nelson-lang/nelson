%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef alphaShape
  properties
    Points = []
    Alpha = Inf
    HoleThreshold = 0
    RegionThreshold = 0
  end

  methods
    function obj = alphaShape(varargin)
      if nargin == 0
        return
      end
      [points, alpha, holeThreshold, regionThreshold] = parseInputs(varargin{:});
      obj.Points = double(points);
      if isempty(alpha)
        obj.Alpha = criticalAlpha(obj, 'all-points');
      else
        obj.Alpha = alpha;
      end
      obj.HoleThreshold = holeThreshold;
      obj.RegionThreshold = regionThreshold;
    end

    function a = alphaSpectrum(obj)
      if isempty(obj.Points)
        a = [];
      else
        span = max(obj.Points, [], 1) - min(obj.Points, [], 1);
        a = unique([0; max(span(:)); Inf]);
      end
    end

    function a = criticalAlpha(obj, varargin)
      if isempty(obj.Points)
        a = NaN;
      else
        span = max(obj.Points, [], 1) - min(obj.Points, [], 1);
        a = max(span(:));
      end
    end

    function n = numRegions(obj)
      n = double(~isempty(obj.Points));
    end

    function tf = inShape(obj, varargin)
      query = parseQuery(varargin{:});
      if isempty(obj.Points)
        tf = false(size(query, 1), 1);
        return
      end
      tri = delaunayn(obj.Points);
      tf = ~isnan(tsearchn(obj.Points, tri, query));
    end

    function tri = alphaTriangulation(obj)
      tri = delaunayn(obj.Points);
    end

    function facets = boundaryFacets(obj)
      facets = convhulln(obj.Points);
    end

    function p = perimeter(obj)
      p = boundaryMeasure(obj, 1);
    end

    function a = area(obj)
      if size(obj.Points, 2) ~= 2
        error('Nelson:geometry:twoDimensionalPointsExpected', 'Two-dimensional points expected.');
      end
      [~, a] = convhull(obj.Points);
    end

    function a = surfaceArea(obj)
      a = boundaryMeasure(obj, 2);
    end

    function v = volume(obj)
      if size(obj.Points, 2) < 3
        v = area(obj);
      else
        [~, v] = convhulln(obj.Points);
      end
    end

    function ids = nearestNeighbor(obj, query)
      ids = dsearchn(obj.Points, query);
    end

    function h = plot(obj)
      facets = boundaryFacets(obj);
      if size(obj.Points, 2) == 2
        ids = orderEdges(facets);
        h = plot(obj.Points(ids, 1), obj.Points(ids, 2));
      else
        h = plot3(obj.Points(:, 1), obj.Points(:, 2), obj.Points(:, 3), '.');
      end
    end
  end
end
%=============================================================================
function [points, alpha, holeThreshold, regionThreshold] = parseInputs(varargin)
  alpha = [];
  holeThreshold = 0;
  regionThreshold = 0;
  while length(varargin) >= 2 && isText(varargin{end - 1})
    name = lower(char(varargin{end - 1}));
    value = varargin{end};
    if strcmp(name, 'holethreshold')
      holeThreshold = value;
    elseif strcmp(name, 'regionthreshold')
      regionThreshold = value;
    else
      error('Nelson:geometry:unsupportedAlphashapeOption', 'Unsupported "%s" option.', 'alphaShape');
    end
    varargin = varargin(1:end - 2);
  end
  if ~isempty(varargin) && isnumeric(varargin{end}) && isscalar(varargin{end}) && nargin > 1
    alpha = varargin{end};
    varargin = varargin(1:end - 1);
  end
  if length(varargin) == 1
    points = varargin{1};
  else
    points = [];
    for k = 1:length(varargin)
      points = [points, varargin{k}(:)];
    end
  end
end
%=============================================================================
function tf = isText(value)
  tf = ischar(value) || (isa(value, 'string') && isscalar(value));
end
%=============================================================================
function query = parseQuery(varargin)
  if length(varargin) == 1
    query = varargin{1};
  else
    query = [];
    for k = 1:length(varargin)
      query = [query, varargin{k}(:)];
    end
  end
end
%=============================================================================
function value = boundaryMeasure(obj, mode)
  facets = boundaryFacets(obj);
  value = 0;
  if size(obj.Points, 2) == 2
    ids = orderEdges(facets);
    p = obj.Points(ids, :);
    d = diff(p, 1, 1);
    value = sum(sqrt(sum(d .* d, 2)));
  elseif mode == 2
    for k = 1:size(facets, 1)
      p = obj.Points(facets(k, 1:3), :);
      value = value + norm(cross(p(2, :) - p(1, :), p(3, :) - p(1, :))) / 2;
    end
  else
    value = NaN;
  end
end
%=============================================================================
function order = orderEdges(edges)
  order = edges(1, :);
  edges(1, :) = [];
  while ~isempty(edges)
    idx = find(edges(:, 1) == order(end) | edges(:, 2) == order(end), 1);
    if isempty(idx)
      break
    end
    if edges(idx, 1) == order(end)
      order(end + 1) = edges(idx, 2);
    else
      order(end + 1) = edges(idx, 1);
    end
    edges(idx, :) = [];
  end
  if order(end) ~= order(1)
    order(end + 1) = order(1);
  end
end
%=============================================================================
