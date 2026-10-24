%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A contour ring follows every grid-cell crossing, interpolated along each cell
% edge: at native grid resolution it is already a dense polyline, not a coarse
% facet polygon, so the drawn curve reads smooth without any display upsampling.
%=============================================================================
% One smooth bump, so each level is a single closed ring. The vertices come from
% the grid crossings, so a ring has about one vertex per cell it passes through
% and its mean segment length is on the order of the grid spacing.
counts = zeros(1, 3);
grids = [40 80 160];
for k = 1:numel(grids)
  N = grids(k);
  x = linspace(-3, 3, N);
  y = x;
  [X, Y] = meshgrid(x, y);
  Z = exp(-(X .^ 2 + Y .^ 2));
  M = contourc(x, y, Z, [0.8 0.8]);
  npts = M(2, 1);
  pts = M(:, 2:1 + npts);
  counts(k) = npts;
  spacing = x(2) - x(1);
  segments = sqrt(diff(pts(1, :)) .^ 2 + diff(pts(2, :)) .^ 2);
  meanSegment = mean(segments);
  % Vertices sit on cell crossings: the mean step stays near one grid cell. A
  % decimated, coarse-facet ring would jump several cells per vertex.
  asserts.istrue(meanSegment < 1.6 * spacing);
  asserts.istrue(meanSegment > 0.25 * spacing);
end
%=============================================================================
% The finest grid resolves the ring with many vertices, not a dozen facets.
asserts.istrue(counts(3) >= 80);
%=============================================================================
% The ring resolution follows the grid resolution: four times the grid gives
% roughly four times the vertices (one crossing per cell), so a denser grid
% draws a smoother ring on its own.
ratio = counts(3) / counts(1);
asserts.istrue(ratio > 3.2);
asserts.istrue(ratio < 5.0);
%=============================================================================
% Even a moderately coarse grid already resolves each closed ring as a dense
% polyline at native resolution, so the drawn contour reads smooth without any
% display-time upsampling of the grid.
x = linspace(-3, 3, 60);
y = x;
[X, Y] = meshgrid(x, y);
Z = exp(-(X .^ 2 + Y .^ 2));
M = contourc(x, y, Z, [0.2 0.5 0.8]);
i = 1;
while i < size(M, 2)
  n = M(2, i);
  % Each bump ring keeps many vertices: a smooth curve, not a handful of facets.
  asserts.istrue(n >= 20);
  i = i + n + 1;
end
%=============================================================================
