%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [idx, bary] = tsearchn(points, tri, query)
  narginchk(3, 3);
  points = double(points);
  tri = double(tri);
  query = double(query);
  if size(query, 2) ~= size(points, 2) && size(query, 1) == size(points, 2)
    query = query.';
  end
  if size(query, 2) ~= size(points, 2)
    error('Nelson:geometry:queryPointsSameNumberColumnsPoints', '%s must have the same number of columns as points.', 'Query points');
  end
  nq = size(query, 1);
  ns = size(tri, 1);
  nv = size(tri, 2);
  nd = size(points, 2);
  idx = NaN(nq, 1);
  bary = NaN(nq, nv);
  tol = 1e-10;
  if nq == 0 || ns == 0
    return
  end
  % Precompute, once per simplex, its bounding box and the transform that maps
  % a point to its barycentric coordinates. A point can only lie in a simplex
  % whose box contains it, so the box test (vectorised over every simplex)
  % skips almost all simplices before the barycentric check, and the
  % precomputed transform replaces the per-test linear solve. This turns the
  % search from O(nq*ns) linear solves into O(nq*ns) cheap box comparisons plus
  % a few matrix-vector products per query.
  square = (nv == nd + 1);
  lo = zeros(ns, nd);
  hi = zeros(ns, nd);
  transforms = cell(ns, 1);
  for s = 1:ns
    verts = points(tri(s, :), :);
    lo(s, :) = min(verts, [], 1);
    hi(s, :) = max(verts, [], 1);
    if square
      transforms{s} = inv([verts, ones(nv, 1)].');
    end
  end
  for q = 1:nq
    pt = query(q, :);
    candidates = find(all(lo <= pt + tol, 2) & all(hi >= pt - tol, 2));
    for ci = 1:numel(candidates)
      s = candidates(ci);
      if square
        lambda = (transforms{s} * [pt, 1].').';
      else
        lambda = simplexBarycentric(points(tri(s, :), :), pt);
      end
      if all(lambda >= -tol) && all(lambda <= 1 + tol)
        idx(q) = s;
        bary(q, :) = lambda;
        break
      end
    end
  end
end
%=============================================================================
function lambda = simplexBarycentric(simplex, point)
  a = [simplex, ones(size(simplex, 1), 1)];
  rhs = [point, 1].';
  lambda = (a.' \ rhs).';
end
%=============================================================================
