%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ADV-CLI MODE-->
%=============================================================================
% Regression: a lit patch must be shaded per face, not rendered as a flat
% silhouette. FaceColor is uniform, so any luminance variation across the patch
% can only come from lighting. This guards the patch lighting path (drawPatch /
% tryPushWebGlPatch, both via RenderLighting::computeLitColor) independently of
% surfaces; before it existed the web backend dropped the normals and drew a
% flat shape. Uses an isosurface sphere so many faces span the brightness range.
%=============================================================================
close all;
[X, Y, Z] = meshgrid(linspace(-1, 1, 24));
V = sqrt(X .^ 2 + Y .^ 2 + Z .^ 2);
fv = isosurface(X, Y, Z, V, 0.8);
spreads = [0, 0];
idx = 0;
for withLight = [false, true]
  idx = idx + 1;
  f = figure('Visible', 'on', 'Position', [100 100 620 460], 'Color', [1 1 1]);
  p = patch('Vertices', fv.vertices, 'Faces', fv.faces, ...
    'FaceColor', [0.30 0.55 0.85], 'EdgeColor', 'none');
  p.FaceLighting = 'flat';
  axis equal;
  axis off;
  view(-37.5, 30);
  if withLight
    camlight('headlight');
    material('dull');
  end
  drawnow();
  F = getframe(f);
  c = double(F.cdata);
  lum = 0.299 * c(:, :, 1) + 0.587 * c(:, :, 2) + 0.114 * c(:, :, 3);
  mask = lum < 250; % patch pixels; the white figure background is ~255
  spreads(idx) = std(lum(mask));
  close(f);
end
close all;
%=============================================================================
unlitSpread = spreads(1);
litSpread = spreads(2);
% The lit patch is shaded, the unlit one is nearly flat (only edge
% antialiasing), and the shading is produced by the light (lit >> unlit).
asserts.istrue(litSpread > 16);
asserts.istrue(unlitSpread < 13);
asserts.istrue(litSpread > 2 * unlitSpread);
%=============================================================================
