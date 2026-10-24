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
% Non-regression: a patch with FaceColor 'interp' and per-vertex FaceVertexCData
% must keep its smooth per-pixel colour gradient when exported through the
% software raster (saveas / print, which paints with RenderWeb). A prior bug
% gated the interpolating z-buffer raster on Gouraud lighting only, so a non-lit
% 'interp' patch fell back to the flat per-face polygon path and collapsed the
% whole quad to a single colour (vertexcolors[0]). An interpolated frame has
% hundreds of distinct colours across a mid-gradient scanline; a flat one a few.
%=============================================================================
close all;
f = figure('Visible', 'off');
set(f, 'Position', [100 100 480 480], 'Color', [1 1 1]);
V = [0 0 0; 1 0 0; 1 1 0; 0 1 0];
F = [1 2 3; 1 3 4];
C = [1 0 0; 0 1 0; 0 0 1; 1 1 0];
patch('Faces', F, 'Vertices', V, 'FaceVertexCData', C, ...
  'FaceColor', 'interp', 'EdgeColor', 'none');
view(2);
axis equal;
axis off;
drawnow();
%=============================================================================
png = [tempname(), '.png'];
saveas(f, png);
asserts.istrue(isfile(png));
C2 = imread(png);
%=============================================================================
% Central horizontal band, away from the transparent margins.
row = round(size(C2, 1) * 0.5);
c1 = round(size(C2, 2) * 0.25);
c2 = round(size(C2, 2) * 0.75);
band = double(squeeze(C2(row, c1:c2, :)));
distinctColors = size(unique(band, 'rows'), 1);
% Interpolated: hundreds of shades. Flat regression: a single colour across the
% whole face. The gap is wide, so a conservative threshold cleanly rejects it.
asserts.istrue(distinctColors > 150);
%=============================================================================
% Adjacent pixels must vary gradually (interpolation), never in flat plateaus:
% the mean absolute step between neighbours is small but non-zero.
steps = abs(diff(band, 1, 1));
meanStep = mean(steps(:));
asserts.istrue(meanStep > 0);
asserts.istrue(meanStep < 20);
%=============================================================================
if isfile(png)
  rmfile(png);
end
close(f);
%=============================================================================
