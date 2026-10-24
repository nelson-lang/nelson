%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
% A perspective camera must produce real foreshortening: with a fixed camera
% view angle, an object farther from the camera must render smaller than the
% same object closer to the camera. With a manually set CameraViewAngle, the
% orthographic view is also scaled by the camera distance (the angle fixes the
% zoom at that distance), so a farther camera renders smaller there too. Only an
% automatic CameraViewAngle makes the orthographic apparent size distance-free.
%=============================================================================
function c = coverage(img)
  img = double(img);
  bg = (img(:, :, 1) > 250) & (img(:, :, 2) > 250) & (img(:, :, 3) > 250);
  c = 1 - (sum(bg(:)) / numel(bg));
end
%=============================================================================
function c = renderSurf(projection, distance)
  [x, y, z] = peaks(40);
  f = figure('Visible', 'on', 'Position', [100 100 500 400], 'Color', [1 1 1]);
  ax = axes('Parent', f);
  surf(ax, x, y, z, 'EdgeColor', 'none');
  set(ax, 'Projection', projection);
  set(ax, 'CameraTarget', [0 0 0]);
  set(ax, 'CameraPosition', [distance * 0.6, -distance * 0.7, distance * 0.4]);
  set(ax, 'CameraViewAngle', 15);
  axis(ax, 'off');
  drawnow();
  c = coverage(getframe(f).cdata);
  close(f);
end
%=============================================================================
close all;
% Setting CameraViewAngle switches the mode to manual (like CameraPosition).
f = figure('Visible', 'off');
surf(peaks(20));
ax = gca();
camva(ax, 20);
asserts.isequal(char(get(ax, 'CameraViewAngleMode')), 'manual');
asserts.isequal(get(ax, 'CameraViewAngle'), 20);
close(f);
%=============================================================================
% Perspective: nearer camera fills much more of the frame than a far camera.
covNear = renderSurf('perspective', 20);
covFar = renderSurf('perspective', 150);
asserts.istrue(covNear > 0.05);
asserts.istrue(covNear > covFar * 3);
%=============================================================================
% Orthographic with a manual CameraViewAngle: nearer camera fills much more of
% the frame than a far camera, as the view angle fixes the zoom at the camera
% distance (matching the reference numerical language).
covOrthoNear = renderSurf('orthographic', 20);
covOrthoFar = renderSurf('orthographic', 150);
asserts.istrue(covOrthoNear > 0.05);
asserts.istrue(covOrthoNear > covOrthoFar * 3);
%=============================================================================
close all;
%=============================================================================
