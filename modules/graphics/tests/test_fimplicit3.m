%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
h = fimplicit3(@(x, y, z) x .^ 2 + y .^ 2 + z .^ 2 - 1, [-1.5 1.5], '--r', ...
  'MeshDensity', 21, 'LineWidth', 2);
asserts.istrue(isgraphics(h, 'implicitfunctionsurface'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.Type, 'implicitfunctionsurface');
asserts.isequal(h.XRange, [-1.5 1.5]);
asserts.isequal(h.YRange, [-1.5 1.5]);
asserts.isequal(h.ZRange, [-1.5 1.5]);
asserts.isequal(h.MeshDensity, 21);
asserts.isequal(h.FaceColor, [1 0 0]);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.Marker, 'none');
asserts.isequal(size(h.Faces, 2), 3);
asserts.isequal(size(h.Vertices, 2), 3);
asserts.istrue(size(h.Faces, 1) > 0);
asserts.istrue(size(h.Vertices, 1) > 0);
asserts.istrue(min(triangleAreas(h.Faces, h.Vertices)) > 0);
ax = ancestor(h, 'axes');
asserts.isequal(ax.View, [-37.5 30]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fimplicit3(@(x, y, z) x .^ 2 + y .^ 2 + z .^ 2 - 1, [-1.5 1.5]);
asserts.isequal(h.FaceColor, 'interp');
asserts.isequal(h.EdgeColor, [0 0 0]);
asserts.isequal(h.MeshDensity, 35);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = fimplicit3(ax, @(x, y, z) scalar_sphere(x, y, z), 'ob', [-1.5 1.5], ...
  'FaceAlpha', 0.5, 'MeshDensity', 11);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.Type, 'implicitfunctionsurface');
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.FaceColor, [0 0 1]);
asserts.isequal(h.FaceAlpha, 0.5);
close(f);
%=============================================================================
asserts.checkerror('fimplicit3(1);', getString(message('Nelson:graphics:functionHandleExpected', 'Value')));
asserts.checkerror('fimplicit3(@(x, y, z) x, [1 1]);', getString(message('Nelson:graphics:rangeNotIncreasing')));
%=============================================================================
function v = scalar_sphere(x, y, z)
  if numel(x) ~= 1 || numel(y) ~= 1 || numel(z) ~= 1
    error('scalar only');
  end
  v = x .^ 2 + y .^ 2 + z .^ 2 - 1;
end
%=============================================================================
function areas = triangleAreas(faces, vertices)
  areas = zeros(size(faces, 1), 1);
  for k = 1:size(faces, 1)
    p1 = vertices(faces(k, 1), :);
    p2 = vertices(faces(k, 2), :);
    p3 = vertices(faces(k, 3), :);
    u = p2 - p1;
    v = p3 - p1;
    normal = [u(2) * v(3) - u(3) * v(2), ...
      u(3) * v(1) - u(1) * v(3), u(1) * v(2) - u(2) * v(1)];
    areas(k) = sqrt(normal(1) .* normal(1) + normal(2) .* normal(2) ...
      + normal(3) .* normal(3)) ./ 2;
  end
end
%=============================================================================
