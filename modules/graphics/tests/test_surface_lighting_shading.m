%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
% Regression: a lit surface must be shaded, not rendered as a flat silhouette.
% FaceColor is uniform, so any luminance variation across the surface can only
% come from lighting (a colormap would vary on its own). This guards the shared
% lighting math (RenderLighting::computeLitColor) used by every raster backend;
% before it existed the web backend dropped the normals and drew a flat shape.
%=============================================================================
reset(groot());
close all;
spreads = [0, 0];
idx = 0;
for withLight = [false, true]
  idx = idx + 1;
  f = figure('Visible', 'on', 'Position', [100 100 620 460], 'Color', [1 1 1]);
  s = surf(peaks(40));
  s.FaceColor = [0.80 0.72 0.35];
  s.EdgeColor = 'none';
  s.FaceLighting = 'gouraud';
  axis off;
  view(-37.5, 30);
  if withLight
    camlight('headlight');
    material('shiny');
  end
  c = captureSurfaceFrame(f);
  lum = 0.299 * c(:, :, 1) + 0.587 * c(:, :, 2) + 0.114 * c(:, :, 3);
  mask = lum < 250; % surface pixels; the white figure background is ~255
  asserts.istrue(nnz(mask) > 100);
  spreads(idx) = std(lum(mask));
  close(f);
end
close all;
%=============================================================================
unlitSpread = spreads(1);
litSpread = spreads(2);
% The lit surface is shaded, the unlit one is nearly flat (only edge
% antialiasing), and the shading is produced by the light (lit >> unlit).
asserts.istrue(litSpread > 25);
asserts.istrue(unlitSpread < 15);
asserts.istrue(litSpread > 3 * unlitSpread);
%=============================================================================
function c = captureSurfaceFrame(fig)
  for k = 1:5
    drawnow();
    F = getframe(fig);
    c = double(F.cdata);
    lum = 0.299 * c(:, :, 1) + 0.587 * c(:, :, 2) + 0.114 * c(:, :, 3);
    if nnz(lum < 250) > 100
      return
    end
    pause(0.05);
  end
end
%=============================================================================
