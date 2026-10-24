%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = [0 0; 1 0; 1 1; 0 1];
%=============================================================================
asserts.isequal(nargout('convhulln'), 2);
asserts.isequal(nargout('delaunayn'), 1);
asserts.isequal(nargout('voronoin'), 2);
asserts.isequal(nargout('convhull'), 2);
asserts.isequal(nargout('boundary'), 2);
asserts.isequal(nargout('dsearchn'), 2);
asserts.isequal(nargout('tsearchn'), 2);
asserts.isequal(nargout('griddata'), 3);
asserts.isequal(nargout('voronoi'), 2);
%=============================================================================
k = convhulln(p);
asserts.isequal(size(k, 2), 2);
asserts.istrue(all(k(:) >= 1));
%=============================================================================
[k2, a] = convhull(p);
asserts.isequal(k2(1), k2(end));
asserts.isapprox(a, 1, 1e-12);
ks = convhull(p, 'Simplify', false);
asserts.isequal(ks(1), ks(end));
%=============================================================================
t = delaunayn(p);
asserts.isequal(size(t, 2), 3);
asserts.istrue(all(t(:) >= 1));
%=============================================================================
t2 = delaunay(p(:, 1), p(:, 2));
asserts.isequal(size(t2, 2), 3);
%=============================================================================
[v, c] = voronoin(p);
asserts.isequal(size(v, 2), 2);
asserts.isequal(length(c), size(p, 1));
[vx, vy] = voronoi(p(:, 1), p(:, 2));
asserts.istrue(~isempty(vx));
asserts.isequal(size(vx), size(vy));
%=============================================================================
q = [0.25 0.25; 2 2];
[idx, bary] = tsearchn(p, t, q);
asserts.istrue(~isnan(idx(1)));
asserts.istrue(isnan(idx(2)));
asserts.isapprox(sum(bary(1, :)), 1, 1e-12);
%=============================================================================
[nearest, dist] = dsearchn(p, q);
asserts.isequal(nearest(1), 1);
asserts.isapprox(dist(1), sqrt(0.125), 1e-12);
[nearestOutside, ~] = dsearchn(p, t, [2 2], Inf);
asserts.isequal(nearestOutside, Inf);
%=============================================================================
vq = griddata(p(:, 1), p(:, 2), p(:, 1) + p(:, 2), 0.25, 0.25);
asserts.isapprox(vq, 0.5, 1e-12);
vq = griddata(p, p(:, 1) + p(:, 2), 0.25, 0.25);
asserts.isapprox(vq, 0.5, 1e-12);
[xq, yq, vq] = griddata(p(:, 1), p(:, 2), p(:, 1) + p(:, 2), 0.25, 0.25);
asserts.isapprox([xq, yq, vq], [0.25, 0.25, 0.5], 1e-12);
%=============================================================================
f = scatteredInterpolant(p, p(:, 1) + p(:, 2), 'nearest');
asserts.isequal(evaluate(f, [0.9 0.9]), 2);
asserts.isequal(f(0.9, 0.9), 2);
%=============================================================================
tr = triangulation(t, p);
asserts.isequal(size(edges(tr), 2), 2);
asserts.isequal(size(freeBoundary(tr), 2), 2);
[loc, bc] = pointLocation(tr, [0.25 0.25]);
asserts.istrue(~isnan(loc));
asserts.isapprox(sum(bc), 1, 1e-12);
triRight = triangulation([1 2 3], [0 0; 2 0; 0 2]);
ic = incenter(triRight);
asserts.isapprox(ic, [2 - sqrt(2), 2 - sqrt(2)], 1e-12);
%=============================================================================
dt = delaunayTriangulation(p);
asserts.isequal(dt.Points, p);
asserts.isequal(size(dt.ConnectivityList, 2), 3);
dtc = delaunayTriangulation(p, [1 2; 2 3]);
asserts.isequal(dtc.Constraints, [1 2; 2 3]);
%=============================================================================
shp = alphaShape(p);
asserts.isapprox(area(shp), 1, 1e-12);
asserts.isequal(numRegions(shp), 1);
asserts.istrue(inShape(shp, [0.25 0.25]));
shp = alphaShape(p, 'HoleThreshold', 3, 'RegionThreshold', 2);
asserts.isequal(shp.HoleThreshold, 3);
asserts.isequal(shp.RegionThreshold, 2);
%=============================================================================
