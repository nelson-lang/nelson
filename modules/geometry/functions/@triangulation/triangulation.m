%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef triangulation
  properties
    Points = []
    ConnectivityList = []
  end

  methods
    function obj = triangulation(connectivity, varargin)
      if nargin == 0
        return
      end
      obj.ConnectivityList = double(connectivity);
      if nargin == 2
        obj.Points = double(varargin{1});
      else
        obj.Points = buildPoints(varargin{:});
      end
    end

    function value = size(obj, varargin)
      if nargin == 1
        value = size(obj.ConnectivityList);
      else
        value = size(obj.ConnectivityList, varargin{1});
      end
    end

    function e = edges(obj)
      e = unique(sort(simplexEdges(obj.ConnectivityList), 2), 'rows');
    end

    function fb = freeBoundary(obj)
      fb = boundaryFaces(obj.ConnectivityList);
    end

    function n = neighbors(obj)
      n = simplexNeighbors(obj.ConnectivityList);
    end

    function ids = nearestNeighbor(obj, query)
      ids = dsearchn(obj.Points, query);
    end

    function [idx, bary] = pointLocation(obj, query)
      [idx, bary] = tsearchn(obj.Points, obj.ConnectivityList, query);
    end

    function cart = barycentricToCartesian(obj, simplexId, bary)
      if isscalar(simplexId)
        vertices = obj.Points(obj.ConnectivityList(simplexId, :), :);
        cart = bary * vertices;
      else
        cart = zeros(length(simplexId), size(obj.Points, 2));
        for k = 1:length(simplexId)
          vertices = obj.Points(obj.ConnectivityList(simplexId(k), :), :);
          cart(k, :) = bary(k, :) * vertices;
        end
      end
    end

    function bary = cartesianToBarycentric(obj, simplexId, query)
      bary = zeros(size(query, 1), size(obj.ConnectivityList, 2));
      for k = 1:size(query, 1)
        vertices = obj.Points(obj.ConnectivityList(simplexId(k), :), :);
        bary(k, :) = localBarycentric(vertices, query(k, :));
      end
    end

    function att = vertexAttachments(obj, vertexIds)
      if nargin < 2
        vertexIds = (1:size(obj.Points, 1)).';
      end
      att = cell(length(vertexIds), 1);
      for k = 1:length(vertexIds)
        [rows, ~] = find(obj.ConnectivityList == vertexIds(k));
        att{k} = unique(rows).';
      end
    end

    function att = edgeAttachments(obj, queryEdges)
      att = cell(size(queryEdges, 1), 1);
      for k = 1:size(queryEdges, 1)
        a = queryEdges(k, 1);
        b = queryEdges(k, 2);
        rows = find(any(obj.ConnectivityList == a, 2) & any(obj.ConnectivityList == b, 2));
        att{k} = rows.';
      end
    end

    function tf = isConnected(obj, simplexIds)
      if nargin < 2
        simplexIds = (1:size(obj.ConnectivityList, 1)).';
      end
      nb = neighbors(obj);
      seen = false(size(obj.ConnectivityList, 1), 1);
      queue = simplexIds(1);
      while ~isempty(queue)
        s = queue(1);
        queue(1) = [];
        if seen(s)
          continue
        end
        seen(s) = true;
        next = nb(s, :);
        next = next(~isnan(next) & next > 0);
        queue = [queue, next(:).'];
      end
      tf = all(seen(simplexIds));
    end

    function c = circumcenter(obj)
      c = simplexCenters(obj, true);
    end

    function c = incenter(obj)
      c = simplexCenters(obj, false);
    end

    function n = faceNormal(obj)
      n = surfaceNormals(obj);
    end

    function n = vertexNormal(obj)
      faceN = surfaceNormals(obj);
      n = zeros(size(obj.Points));
      for k = 1:size(obj.ConnectivityList, 1)
        ids = obj.ConnectivityList(k, :);
        n(ids, :) = n(ids, :) + faceN(k, :);
      end
      scale = sqrt(sum(n .* n, 2));
      scale(scale == 0) = 1;
      n = n ./ scale;
    end

    function e = featureEdges(obj, varargin)
      e = edges(obj);
    end
  end
end
%=============================================================================
function points = buildPoints(varargin)
  points = [];
  for k = 1:nargin
    points = [points, varargin{k}(:)];
  end
end
%=============================================================================
function e = simplexEdges(t)
  nv = size(t, 2);
  e = [];
  for i = 1:nv - 1
    for j = i + 1:nv
      e = [e; t(:, [i j])];
    end
  end
end
%=============================================================================
function faces = boundaryFaces(t)
  nv = size(t, 2);
  faces = [];
  for omit = 1:nv
    keep = setdiff(1:nv, omit);
    faces = [faces; sort(t(:, keep), 2)];
  end
  [u, ~, ic] = unique(faces, 'rows');
  counts = zeros(size(u, 1), 1);
  for k = 1:length(ic)
    counts(ic(k)) = counts(ic(k)) + 1;
  end
  faces = u(counts == 1, :);
end
%=============================================================================
function nb = simplexNeighbors(t)
  ns = size(t, 1);
  nv = size(t, 2);
  nb = NaN(ns, nv);
  for a = 1:ns - 1
    for b = a + 1:ns
      shared = intersect(t(a, :), t(b, :));
      if length(shared) == nv - 1
        ia = find(isnan(nb(a, :)), 1);
        ib = find(isnan(nb(b, :)), 1);
        nb(a, ia) = b;
        nb(b, ib) = a;
      end
    end
  end
end
%=============================================================================
function lambda = localBarycentric(simplex, point)
  a = [simplex, ones(size(simplex, 1), 1)];
  lambda = (a.' \ [point, 1].').';
end
%=============================================================================
function c = simplexCenters(obj, circum)
  ns = size(obj.ConnectivityList, 1);
  dim = size(obj.Points, 2);
  c = zeros(ns, dim);
  for k = 1:ns
    p = obj.Points(obj.ConnectivityList(k, :), :);
    if circum && size(p, 1) == dim + 1
      c(k, :) = circumcenterSimplex(p);
    elseif ~circum && size(p, 1) == dim + 1
      c(k, :) = incenterSimplex(p);
    else
      c(k, :) = mean(p, 1);
    end
  end
end
%=============================================================================
function c = circumcenterSimplex(p)
  base = p(1, :);
  a = 2 * (p(2:end, :) - base);
  b = sum(p(2:end, :) .^ 2, 2) - sum(base .^ 2);
  c = (a \ b).';
end
%=============================================================================
function c = incenterSimplex(p)
  nv = size(p, 1);
  weights = zeros(nv, 1);
  for k = 1:nv
    facet = p(setdiff(1:nv, k), :);
    weights(k) = simplexFacetMeasure(facet);
  end
  if all(weights == 0)
    c = mean(p, 1);
  else
    c = (weights.' * p) ./ sum(weights);
  end
end
%=============================================================================
function value = simplexFacetMeasure(p)
  if size(p, 1) == 1
    value = 1;
  elseif size(p, 1) == 2
    delta = p(2, :) - p(1, :);
    value = sqrt(sum(delta .* delta));
  elseif size(p, 1) == 3 && size(p, 2) == 3
    value = norm(cross(p(2, :) - p(1, :), p(3, :) - p(1, :))) / 2;
  else
    a = p(2:end, :) - p(1, :);
    value = sqrt(det(a * a.')) / factorial(size(p, 1) - 1);
  end
end
%=============================================================================
function n = surfaceNormals(obj)
  if size(obj.Points, 2) ~= 3 || size(obj.ConnectivityList, 2) < 3
    n = zeros(size(obj.ConnectivityList, 1), size(obj.Points, 2));
    return
  end
  n = zeros(size(obj.ConnectivityList, 1), 3);
  for k = 1:size(obj.ConnectivityList, 1)
    ids = obj.ConnectivityList(k, 1:3);
    a = obj.Points(ids(2), :) - obj.Points(ids(1), :);
    b = obj.Points(ids(3), :) - obj.Points(ids(1), :);
    n(k, :) = cross(a, b);
  end
end
%=============================================================================
