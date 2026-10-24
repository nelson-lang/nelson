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
[x, y] = meshgrid(1:6, 1:6);
z = peaks(6);
T = delaunay(x, y);
f = figure('Visible', 'off');
h = trisurf(T, x, y, z);
asserts.istrue(isgraphics(h, 'patch'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.FaceColor, 'flat');
asserts.isequal(size(h.Faces), size(T));
asserts.isequal(size(h.Vertices), [numel(x), 3]);
asserts.isequal(size(h.FaceVertexCData), [size(T, 1), 3]);
differentColorFound = false;
for k = 2:size(h.FaceVertexCData, 1)
  if ~isequal(h.FaceVertexCData(k, :), h.FaceVertexCData(1, :))
    differentColorFound = true;
    break
  end
end
asserts.istrue(differentColorFound);
%=============================================================================
h = trisurf(T, x, y, z, 'FaceAlpha', 0.4);
asserts.istrue(isgraphics(h, 'patch'));
asserts.isapprox(h.FaceAlpha, 0.4, 1e-12);
%=============================================================================
h = trisurf(T, x, y, z, 'FaceC', 'w', 'EdgeC', 'none', 'AmbientS', 0, ...
  'DiffuseS', 0.6, 'SpecularS', 1);
asserts.istrue(isgraphics(h, 'patch'));
asserts.isequal(h.FaceColor, [1 1 1]);
asserts.isequal(h.EdgeColor, 'none');
asserts.isapprox(h.AmbientStrength, 0, 1e-12);
asserts.isapprox(h.DiffuseStrength, 0.6, 1e-12);
asserts.isapprox(h.SpecularStrength, 1, 1e-12);
set(gca, 'color', 'k');
asserts.isequal(gca().Color, [0 0 0]);
%=============================================================================
h = trisurf(T, x, y, z, 'FaceColor', [0.8 0.8 1.0], 'EdgeColor', 'k');
lighting(gca(), 'gouraud');
asserts.istrue(isgraphics(h, 'patch'));
asserts.isequal(h.FaceColor, [0.8 0.8 1.0]);
asserts.isequal(h.EdgeColor, [0 0 0]);
asserts.isequal(h.FaceLighting, 'gouraud');
asserts.isequal(h.EdgeLighting, 'gouraud');
%=============================================================================
h = trisurf(T, x, y, z, z);
asserts.istrue(isgraphics(h, 'patch'));
asserts.isequal(size(h.FaceVertexCData), [size(T, 1), 3]);
%=============================================================================
TO = triangulation(T, x(:), y(:), z(:));
h = trisurf(TO);
asserts.istrue(isgraphics(h, 'patch'));
%=============================================================================
P = randn(30, 3);
T = convhulln(P);
h = trisurf(T, P(:, 1), P(:, 2), P(:, 3), 'FaceAlpha', 0.4);
asserts.istrue(isgraphics(h, 'patch'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(size(h.Faces), size(T));
asserts.isequal(size(h.Vertices), size(P));
asserts.isapprox(h.FaceAlpha, 0.4, 1e-12);
%=============================================================================
figure('Visible', 'off');
txt = evalc('trisurf(T, P(:, 1), P(:, 2), P(:, 3), ''FaceAlpha'', 0.4)');
asserts.isfalse(contains(txt, 'ans ='));
%=============================================================================
% per-vertex color data is accepted in any orientation: the coordinates are
% row vectors here while the color is a column vector of the same length
figure('Visible', 'off');
tri = [1 2 3; 1 3 4];
xr = [0 1 1 0];
yr = [0 0 1 1];
zr = [0 0 1 1];
cCol = [0.1; 0.2; 0.3; 0.4];
h = trisurf(tri, xr, yr, zr, cCol);
asserts.istrue(isgraphics(h, 'patch'));
cRow = [0.1 0.2 0.3 0.4];
h2 = trisurf(tri, xr, yr, zr, cRow);
asserts.istrue(isgraphics(h2, 'patch'));
%=============================================================================
% a color vector whose length differs from the number of vertices is rejected
assert_checkerror('trisurf([1 2 3; 1 3 4], [0 1 1 0], [0 0 1 1], [0 0 1 1], [1 2 3])', getString(message('Nelson:dimagree:colorDataSizeZ')));
%=============================================================================
