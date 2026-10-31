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
% Patches in a 3D axes must be depth-ordered against surfaces per pixel in the
% software capture (getframe / saveas), whatever the camera pose and the draw
% order. Scene: a lit sphere (surface) at y = 5, four flat discs (patches) at
% y = 8 (red; green with FaceLighting none; blue with FaceAlpha 0.5; yellow
% inside an hgtransform raised by 1.5), a magenta backdrop patch at y = 12 and
% a cyan backdrop surface at y = 14, viewed from y < 0 towards +y. Expected, in
% every mode below: the discs in front of the magenta plane, the magenta plane
% hiding the cyan one except for a border, the yellow disc higher than the red
% one, the blue disc blended with the magenta behind it.
%
% Before the fix the cyan surface image was always composited over the patches
% (perspective: the five patches vanished; orthographic: cyan drawn over the
% magenta), and a manual CameraPosition with an automatic CameraTarget rendered
% the scene through the 2D path.
%=============================================================================
% Manual camera (position, target, up, view angle), perspective.
[f, ax] = localScene();
set(ax, 'Projection', 'perspective', 'CameraPosition', [0 -6 2], 'CameraTarget', [0 8 2], ...
  'CameraUpVector', [0 0 1], 'CameraViewAngle', 40);
c = localFrame(f);
localAssertDepthOrder(c, 500);
% translucency: the blue disc is blended over the magenta plane (purple), no
% opaque blue is left.
asserts.istrue(nnz(localMask(c, 'purple')) > 500);
asserts.istrue(nnz(localMask(c, 'blue')) < 200);
% the lit sphere (nearest object) is visible.
asserts.istrue(nnz(localMask(c, 'orange')) > 500);
close(f);
%=============================================================================
% Same camera, orthographic: the cyan plane must stay behind the magenta one.
[f, ax] = localScene();
set(ax, 'Projection', 'orthographic', 'CameraPosition', [0 -6 2], 'CameraTarget', [0 8 2], ...
  'CameraUpVector', [0 0 1], 'CameraViewAngle', 40);
c = localFrame(f);
localAssertDepthOrder(c, 500);
close(f);
%=============================================================================
% Manual CameraPosition only (automatic target at the centre of the data box):
% the camera is posed, so the axes must not render through the 2D path (the
% magenta plane filled the whole frame).
[f, ax] = localScene();
set(ax, 'Projection', 'perspective', 'CameraPosition', [0 -6 2], 'CameraUpVector', [0 0 1], ...
  'CameraViewAngle', 40);
c = localFrame(f);
localAssertDepthOrder(c, 500);
asserts.istrue(nnz(localMask(c, 'orange')) > 500);
asserts.istrue(nnz(localMask(c, 'magenta')) < 0.9 * size(c, 1) * size(c, 2));
close(f);
%=============================================================================
% Automatic camera from view(0, 2) with a manual view angle: the scene is tiny
% but every layer must still be there.
[f, ax] = localScene();
set(ax, 'Projection', 'perspective', 'CameraViewAngle', 40);
view(ax, 0, 2);
c = localFrame(f);
localAssertDepthOrder(c, 20);
close(f);
%=============================================================================
% The boing_ball_3d configuration (axis equal, perspective, view(32, 18)).
[f, ax] = localScene();
axis(ax, 'equal');
axis(ax, [-10 10 -5 15 -5 8]);
axis(ax, 'off');
set(ax, 'Projection', 'perspective');
view(ax, 32, 18);
c = localFrame(f);
localAssertDepthOrder(c, 200);
close(f);
%=============================================================================
function [f, ax] = localScene()
  f = figure('Visible', 'off', 'Color', [0 0 0], 'Position', [50 50 800 500]);
  ax = axes('Parent', f, 'Position', [0 0 1 1], 'Color', [0.1 0.1 0.3]);
  hold(ax, 'on');
  [sx, sy, sz] = sphere(24);
  surf(ax, sx, sy + 5, sz + 1, 'FaceColor', [1 0.5 0], 'EdgeColor', 'none', 'FaceLighting', 'gouraud');
  light(ax, 'Position', [2 -5 5], 'Style', 'local');
  a = linspace(0, 2 * pi, 40);
  patch('Parent', ax, 'XData', -4 + 1.2 * cos(a), 'YData', 8 * ones(size(a)), 'ZData', 2 + 1.2 * sin(a), ...
    'FaceColor', [1 0 0], 'EdgeColor', 'none');
  patch('Parent', ax, 'XData', -1.5 + 1.2 * cos(a), 'YData', 8 * ones(size(a)), 'ZData', 2 + 1.2 * sin(a), ...
    'FaceColor', [0 1 0], 'EdgeColor', 'none', 'FaceLighting', 'none');
  patch('Parent', ax, 'XData', 1 + 1.2 * cos(a), 'YData', 8 * ones(size(a)), 'ZData', 2 + 1.2 * sin(a), ...
    'FaceColor', [0.2 0.4 1], 'EdgeColor', 'none', 'FaceLighting', 'none', 'FaceAlpha', 0.5);
  g = hgtransform('Parent', ax);
  patch('Parent', g, 'XData', 3.5 + 1.2 * cos(a), 'YData', 8 * ones(size(a)), 'ZData', 2 + 1.2 * sin(a), ...
    'FaceColor', [1 1 0], 'EdgeColor', 'none', 'FaceLighting', 'none');
  set(g, 'Matrix', makehgtform('translate', [0 0 1.5]));
  patch('Parent', ax, 'XData', [-8 8 8 -8], 'YData', [12 12 12 12], 'ZData', [-3 -3 6 6], ...
    'FaceColor', [1 0 1], 'EdgeColor', 'none', 'FaceLighting', 'none');
  [bx, bz] = meshgrid(linspace(-10, 10, 5), linspace(-4, 7, 5));
  surf(ax, bx, 14 * ones(size(bx)), bz, 'FaceColor', [0 1 1], 'EdgeColor', 'none', 'FaceLighting', 'none');
  hold(ax, 'off');
  set(ax, 'DataAspectRatio', [1 1 1], 'XLim', [-10 10], 'YLim', [-5 15], 'ZLim', [-5 8], ...
    'XLimMode', 'manual', 'YLimMode', 'manual', 'ZLimMode', 'manual', 'Visible', 'off');
end
%=============================================================================
function c = localFrame(f)
  drawnow();
  fr = getframe(f);
  c = fr.cdata;
end
%=============================================================================
function localAssertDepthOrder(c, minDiscPixels)
  % Every layer is visible ...
  red = localMask(c, 'red');
  yellow = localMask(c, 'yellow');
  magenta = localMask(c, 'magenta');
  cyan = localMask(c, 'cyan');
  asserts.istrue(nnz(red) > minDiscPixels);
  asserts.istrue(nnz(localMask(c, 'green')) > minDiscPixels);
  asserts.istrue(nnz(yellow) > minDiscPixels);
  asserts.istrue(nnz(magenta) > 10 * minDiscPixels);
  asserts.istrue(nnz(cyan) > minDiscPixels);
  % ... the magenta plane (nearer) hides the cyan one (farther, larger): only a
  % cyan border remains, much smaller than the magenta area.
  asserts.istrue(nnz(magenta) > nnz(cyan));
  % ... and the hgtransform raised the yellow disc above the red one.
  [ry, rx] = localCentroid(red);
  [yy, yx] = localCentroid(yellow);
  asserts.istrue(yy < ry - 0.01 * size(c, 1));
  asserts.istrue(yx > rx);
end
%=============================================================================
function mask = localMask(c, name)
  r = double(c(:, :, 1));
  g = double(c(:, :, 2));
  b = double(c(:, :, 3));
  switch name
    case 'red'
      mask = r > 150 & g < 100 & b < 100;
    case 'green'
      mask = r < 100 & g > 150 & b < 100;
    case 'blue'
      mask = r < 90 & g > 70 & g < 140 & b > 200;
    case 'yellow'
      mask = r > 200 & g > 200 & b < 100;
    case 'magenta'
      mask = r > 200 & g < 80 & b > 200;
    case 'cyan'
      mask = r < 80 & g > 200 & b > 200;
    case 'purple'
      % 0.5 * [0.2 0.4 1] + 0.5 * [1 0 1]
      mask = r > 110 & r < 200 & g > 15 & g < 90 & b > 220;
    case 'orange'
      mask = r > 180 & g > 60 & g < 200 & b < 80;
  end
end
%=============================================================================
function [cy, cx] = localCentroid(mask)
  [rows, cols] = find(mask);
  cy = mean(rows);
  cx = mean(cols);
end
%=============================================================================
