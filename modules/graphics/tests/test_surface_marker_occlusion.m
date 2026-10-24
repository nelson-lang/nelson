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
% <--WITH DISPLAY-->
%=============================================================================
% A distant set of point markers behind an opaque surface must be occluded by
% it, not pierce through to the foreground. A ring of white points far behind a
% central orange sphere must leave the sphere's silhouette clean (no white
% pixels over it), while the points outside the silhouette stay visible. Both
% render backends are checked: the on-screen OpenGL frame (what the user sees)
% and the software RenderWeb frame (getframe / saveas).
%=============================================================================
function n = whiteInCenter(img)
  % Bright (marker) pixels inside the central disk the sphere covers.
  img = double(img);
  h = size(img, 1);
  w = size(img, 2);
  [X, Y] = meshgrid(1:w, 1:h);
  d = sqrt((X - w / 2) .^ 2 + (Y - h / 2) .^ 2);
  mask = d < min(h, w) * 0.17;
  white = (img(:, :, 1) > 200) & (img(:, :, 2) > 200) & (img(:, :, 3) > 200) & mask;
  n = sum(white(:));
end
%=============================================================================
function n = whiteOutside(img)
  % Bright (marker) pixels outside the sphere silhouette: the still-visible ones.
  img = double(img);
  h = size(img, 1);
  w = size(img, 2);
  [X, Y] = meshgrid(1:w, 1:h);
  d = sqrt((X - w / 2) .^ 2 + (Y - h / 2) .^ 2);
  mask = d > min(h, w) * 0.30;
  white = (img(:, :, 1) > 200) & (img(:, :, 2) > 200) & (img(:, :, 3) > 200) & mask;
  n = sum(white(:));
end
%=============================================================================
function f = buildScene()
  f = figure('Visible', 'on', 'Color', [0 0 0], 'Position', [60 60 700 700]);
  ax = axes('Parent', f);
  hold(ax, 'on');
  axis(ax, 'equal');
  axis(ax, 'off');
  set(ax, 'Color', [0 0 0], 'Projection', 'perspective', 'Clipping', 'off', ...
    'Position', [0 0 1 1]);
  [sx, sy, sz] = sphere(40);
  surf(ax, 20 * sx, 20 * sy, 20 * sz, 'FaceColor', [0.8 0.5 0.2], ...
    'EdgeColor', 'none', 'FaceLighting', 'none');
  % Deterministic ring of points on a far sphere (radius 300): they englobe the
  % central sphere, so half project onto its silhouette and must be occluded.
  theta = linspace(0, 2 * pi, 40);
  phi = linspace(0.15, pi - 0.15, 24);
  [T, P] = meshgrid(theta, phi);
  R = 300;
  px = R .* sin(P(:)') .* cos(T(:)');
  py = R .* sin(P(:)') .* sin(T(:)');
  pz = R .* cos(P(:)');
  % '.' renders at a fraction of the nominal MarkerSize, so use a large value
  % to leave a comfortable number of visible marker pixels outside the sphere.
  plot3(ax, px, py, pz, '.', 'Color', [1 1 1], 'MarkerSize', 12);
  set(ax, 'CameraPositionMode', 'manual', 'CameraTargetMode', 'manual', ...
    'CameraUpVectorMode', 'manual');
  campos(ax, [0 -120 40]);
  camtarget(ax, [0 0 0]);
  camup(ax, [0 0 1]);
  camva(ax, 40);
end
%=============================================================================
% RenderWeb backend (software, headless-safe): the sphere silhouette must stay
% clean while markers outside it remain visible.
f = buildScene();
drawnow();
web = __getframe__(f, false);
assert_istrue(whiteInCenter(web) == 0);
% The exact number of bright marker pixels depends on marker point size,
% anti-aliasing and display scaling (the '.' point marker renders at a third of
% the nominal MarkerSize), so only assert that a substantial set of markers
% outside the silhouette stays visible rather than an exact count.
assert_istrue(whiteOutside(web) > 20);
close(f);
%=============================================================================
% OpenGL backend (on-screen): same occlusion. Needs an exposed window; skip if
% the frame comes back black (no accessible desktop in this environment).
f = buildScene();
for k = 1:30
  drawnow();
  refresh(f);
end
gl = __getframe__(f, true);
if max(gl(:)) < 8
  skip_testsuite(true, 'OpenGL frame capture returned a black frame.');
end
assert_istrue(whiteInCenter(gl) == 0);
assert_istrue(whiteOutside(gl) > 20);
close(f);
%=============================================================================
