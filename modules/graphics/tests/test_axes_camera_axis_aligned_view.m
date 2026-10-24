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
% A camera looking straight down an axis (top/bottom/side view) must still
% render the scene. When the auto up-vector is collinear with the viewing
% direction the camera basis used to collapse to a null matrix, producing a
% blank frame. This happened in orthographic and perspective projection alike.
%=============================================================================
[x, y, z] = peaks(40);
positions = {[0 0 40], [0 0 -40], [40 0 0], [-40 0 0], [0 40 0], [0 -40 0]};
for k = 1:numel(positions)
  coverage = axis_aligned_coverage(x, y, z, positions{k}, 'orthographic');
  asserts.istrue(coverage > 0.05);
end
%=============================================================================
% Same guard must hold in perspective projection.
coverage = axis_aligned_coverage(x, y, z, [0 0 40], 'perspective');
asserts.istrue(coverage > 0.05);
%=============================================================================
% view(2) is the top-down alias and must render as well.
f = figure('Visible', 'on', 'Position', [100 100 500 400], 'Color', [1 1 1]);
ax = axes('Parent', f);
surf(ax, x, y, z, 'EdgeColor', 'none');
view(ax, 2);
axis(ax, 'off');
drawnow();
coverage = frame_coverage(getframe(f).cdata);
close(f);
asserts.istrue(coverage > 0.05);
%=============================================================================
% An oblique view must keep rendering (no regression on the normal path).
f = figure('Visible', 'on', 'Position', [100 100 500 400], 'Color', [1 1 1]);
ax = axes('Parent', f);
surf(ax, x, y, z, 'EdgeColor', 'none');
view(ax, 35, 30);
axis(ax, 'off');
drawnow();
coverage = frame_coverage(getframe(f).cdata);
close(f);
asserts.istrue(coverage > 0.02);
%=============================================================================
% Web backend: getframe exercises only the Qt backend, so verify the web
% renderer's camera basis directly. A degenerate lookAt would leave the mesh
% model matrix with null rows (zero determinant) and nothing would draw.
f = figure('Visible', 'off', 'Position', [100 100 500 400], 'Color', [1 1 1]);
ax = axes('Parent', f);
surf(ax, x, y, z, 'EdgeColor', 'none');
set(ax, 'CameraTarget', [0 0 0]);
set(ax, 'CameraPosition', [0 0 40]);
axis(ax, 'off');
drawnow();
displayList = jsondecode(__web_display_list__(f));
close(f);
meshes = displayList.webglMeshes;
if iscell(meshes)
  firstMesh = meshes{1};
else
  firstMesh = meshes(1);
end
cameraBasis = reshape(firstMesh.model, 4, 4);
asserts.istrue(abs(det(cameraBasis(1:3, 1:3))) > 1e-6);
%=============================================================================
% The stored auto CameraUpVector must never be collinear with the real viewing
% direction (CameraTarget - CameraPosition). Setting CameraPosition manually
% along an axis does not update View, so the View-derived up used to stay
% (0 0 1) -- collinear with a straight down-z look. The auto up keeps z up for a
% side look and falls back to y for a look straight along z: a top view reports
% [0 1 0] (as view(2) does), a bottom view [0 -1 0] (as view(0, -90) does), and
% x/y side views keep [0 0 1].
positions = {[0 0 40], [0 0 -40], [40 0 0], [-40 0 0], [0 40 0], [0 -40 0]};
expected = {[0 1 0], [0 -1 0], [0 0 1], [0 0 1], [0 0 1], [0 0 1]};
for k = 1:numel(positions)
  up = stored_camera_up(x, y, z, positions{k});
  forward = [0 0 0] - positions{k};
  forward = forward / norm(forward);
  asserts.istrue(abs(dot(up / norm(up), forward)) < 0.9999);
  asserts.istrue(max(abs(up - expected{k})) < 1e-9);
end
%=============================================================================
% An oblique view keeps the conventional (0 0 1) up-vector (no regression).
f = figure('Visible', 'off', 'Position', [100 100 500 400], 'Color', [1 1 1]);
ax = axes('Parent', f);
surf(ax, x, y, z, 'EdgeColor', 'none');
view(ax, 35, 30);
drawnow();
up = get(ax, 'CameraUpVector');
close(f);
asserts.istrue(max(abs(up - [0 0 1])) < 1e-9);
%=============================================================================
function up = stored_camera_up(x, y, z, cameraPosition)
  f = figure('Visible', 'off', 'Position', [100 100 500 400], 'Color', [1 1 1]);
  ax = axes('Parent', f);
  surf(ax, x, y, z, 'EdgeColor', 'none');
  set(ax, 'CameraTarget', [0 0 0]);
  set(ax, 'CameraPosition', cameraPosition);
  drawnow();
  up = get(ax, 'CameraUpVector');
  close(f);
end
%=============================================================================
function coverage = axis_aligned_coverage(x, y, z, cameraPosition, projection)
  f = figure('Visible', 'on', 'Position', [100 100 500 400], 'Color', [1 1 1]);
  ax = axes('Parent', f);
  surf(ax, x, y, z, 'EdgeColor', 'none');
  set(ax, 'Projection', projection);
  set(ax, 'CameraTarget', [0 0 0]);
  set(ax, 'CameraPosition', cameraPosition);
  axis(ax, 'off');
  drawnow();
  coverage = frame_coverage(getframe(f).cdata);
  close(f);
end
%=============================================================================
function coverage = frame_coverage(cdata)
  img = double(cdata);
  bg = (img(:, :, 1) > 250) & (img(:, :, 2) > 250) & (img(:, :, 3) > 250);
  coverage = 1 - sum(bg(:)) / numel(bg);
end
%=============================================================================
