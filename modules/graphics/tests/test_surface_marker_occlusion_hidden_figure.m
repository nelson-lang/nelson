%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% getframe of a figure created with Visible 'off' renders through the QPainter
% software backend (RenderQt), which has no hardware depth buffer.
%=============================================================================
function [f, ax] = buildWall()
  f = figure('Visible', 'off', 'Color', [0 0 0], 'Position', [60 60 800 500]);
  ax = axes('Parent', f, 'Position', [0 0 1 1]);
  hold(ax, 'on');
  [wx, wz] = meshgrid(linspace(-6, 6, 3), linspace(-6, 6, 3));
  surf(ax, wx, 10 * ones(size(wx)), wz, 'FaceColor', [1 0.5 0], ...
    'EdgeColor', 'none', 'FaceLighting', 'none');
end
%=============================================================================
function poseCamera(ax)
  hold(ax, 'off');
  axis(ax, 'equal');
  set(ax, 'Visible', 'off', 'Projection', 'perspective', ...
    'CameraPositionMode', 'manual', 'CameraTargetMode', 'manual', ...
    'CameraUpVectorMode', 'manual', 'CameraViewAngleMode', 'manual', ...
    'CameraPosition', [0 -6 0], 'CameraTarget', [0 8 0], ...
    'CameraUpVector', [0 0 1], 'CameraViewAngle', 40);
end
%=============================================================================
function [mx, my, mz] = markerCloud()
  [gx, gz] = meshgrid(linspace(-30, 30, 25), linspace(-20, 20, 12));
  mx = gx(:)';
  mz = gz(:)';
  my = 30 * ones(size(mx));
end
%=============================================================================
function [inside, outside, wall] = countPixels(img)
  img = double(img);
  h = size(img, 1);
  w = size(img, 2);
  bright = (img(:, :, 1) > 200) & (img(:, :, 2) > 200) & (img(:, :, 3) > 150);
  orange = (img(:, :, 1) > 200) & (img(:, :, 2) > 90) & (img(:, :, 2) < 170) & ...
    (img(:, :, 3) < 60);
  rows = (round(0.1 * h) + 1):round(0.9 * h);
  cols = (round(0.25 * w) + 1):round(0.75 * w);
  inside = nnz(bright(rows, cols));
  wall = nnz(orange(rows, cols)) / (numel(rows) * numel(cols));
  sideCols = [1:round(0.15 * w), (round(0.85 * w) + 1):w];
  outside = nnz(bright(:, sideCols));
end
%=============================================================================
% plot3 '.' markers
[f, ax] = buildWall();
[mx, my, mz] = markerCloud();
plot3(ax, mx, my, mz, '.', 'Color', [1 1 0.8], 'MarkerSize', 12);
poseCamera(ax);
drawnow();
img = getframe(f).cdata;
[inside, outside, wall] = countPixels(img);
% the wall really covers the central box
assert_istrue(wall > 0.9);
% markers outside the wall silhouette stay visible
assert_istrue(outside > 20);
% no marker pierces through the wall
assert_isequal(inside, 0);
close(f);
%=============================================================================
% scatter3 markers
[f, ax] = buildWall();
[mx, my, mz] = markerCloud();
scatter3(ax, mx, my, mz, 24, [1 1 0.8], 'filled');
poseCamera(ax);
drawnow();
img = getframe(f).cdata;
[inside, outside, wall] = countPixels(img);
assert_istrue(wall > 0.9);
assert_istrue(outside > 20);
assert_isequal(inside, 0);
close(f);
%=============================================================================
