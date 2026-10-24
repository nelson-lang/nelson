%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% Perspective projection with a MANUALLY posed oblique camera and a MANUAL
% CameraViewAngle: the apparent size of the subject must depend only on the
% camera distance and the view angle, never on the data extent.
%
% Posing the camera through CameraPosition/CameraTarget does not refresh View,
% so the axes used to be classified as a flat 2D (top-down) view and fell back
% to the orthographic path, whose scale is tied to the axis limits. Adding far
% geometry (here rings of points at radius 200) then ballooned the limits and
% shrank the foreground subject to a speck. With the fix the scene renders in
% true perspective and the subject keeps its size regardless of the far points.
%=============================================================================
% getframe: subject coverage must be (nearly) independent of far geometry.
coverageNear = perspective_subject_coverage(false);
coverageFar = perspective_subject_coverage(true);
asserts.istrue(coverageNear > 0.08);
asserts.istrue(abs(coverageFar - coverageNear) / coverageNear < 0.15);
%=============================================================================
% Web backend: a manually posed oblique camera with far geometry present must
% still build a perspective frustum (w-row == [0 0 -1 0]), not an orthographic
% projection (whose last matrix element is 1).
P = manual_camera_projection();
asserts.istrue(abs(P(4, 4)) < 1e-6);
asserts.istrue(P(4, 3) < -0.5);
%=============================================================================
function coverage = perspective_subject_coverage(addFar)
  f = figure('Visible', 'on', 'Position', [100 100 500 400], 'Color', [0 0 0]);
  ax = build_perspective_subject(f, addFar);
  drawnow();
  coverage = subject_fraction(getframe(f).cdata);
  close(f);
end
%=============================================================================
function P = manual_camera_projection()
  f = figure('Visible', 'off', 'Position', [100 100 500 400], 'Color', [0 0 0]);
  build_perspective_subject(f, true);
  drawnow();
  displayList = jsondecode(__web_display_list__(f));
  close(f);
  meshes = displayList.webglMeshes;
  if iscell(meshes)
    firstMesh = meshes{1};
  else
    firstMesh = meshes(1);
  end
  P = reshape(firstMesh.projection, 4, 4);
end
%=============================================================================
function ax = build_perspective_subject(f, addFar)
  ax = axes('Parent', f);
  hold(ax, 'on');
  axis(ax, 'equal');
  axis(ax, 'off');
  set(ax, 'Color', [0 0 0], 'Clipping', 'off', 'Projection', 'perspective');
  [sx, sy, sz] = sphere(30);
  surf(ax, sx, sy, sz, 'EdgeColor', 'none', 'FaceColor', [1 1 1], ...
       'FaceLighting', 'none');
  if addFar
    t = linspace(0, 2 * pi, 80);
    plot3(ax, 200 * cos(t), 200 * sin(t), 200 * ones(size(t)), '.', ...
          'Color', [0.4 0.4 0.4]);
    plot3(ax, -200 * ones(size(t)), 200 * cos(t), 200 * sin(t), '.', ...
          'Color', [0.4 0.4 0.4]);
  end
  set(ax, 'CameraPositionMode', 'manual', 'CameraTargetMode', 'manual', ...
          'CameraUpVectorMode', 'manual');
  campos(ax, [3 -3 2.4]);
  camtarget(ax, [0 0 0]);
  camup(ax, [0 0 1]);
  camva(ax, 40);
end
%=============================================================================
function frac = subject_fraction(cdata)
  % Count only the bright white sphere; the far points are mid-grey (0.4) and
  % the background is black, so both are excluded.
  img = double(cdata);
  lit = (img(:, :, 1) > 180) & (img(:, :, 2) > 180) & (img(:, :, 3) > 180);
  frac = sum(lit(:)) / (size(img, 1) * size(img, 2));
end
%=============================================================================
