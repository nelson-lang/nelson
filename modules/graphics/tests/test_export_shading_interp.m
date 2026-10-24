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
% Non-regression: exporting a `shading interp` surface through the software
% raster (saveas / print, which paints with RenderWeb) must keep the smooth
% per-pixel colour gradient. A prior bug gated the interpolating z-buffer raster
% on !webglSurface; since the WebGL geometry pass always succeeds, that raster
% was dead and every captured surface fell back to the flat per-quad polygon
% path, collapsing the gradient to one colour per cell. An interpolated frame
% has hundreds of distinct colours across a mid-gradient scanline; a flat one
% has only a handful.
%=============================================================================
close all;
f = figure('Visible', 'off');
set(f, 'Position', [100 100 480 480], 'Color', [1 1 1]);
pcolor(peaks(30));
shading interp;
colormap(parula);
axis off;
drawnow();
%=============================================================================
png = [tempname(), '.png'];
saveas(f, png);
asserts.istrue(isfile(png));
C = imread(png);
%=============================================================================
% Central horizontal band, away from the transparent margins.
row = round(size(C, 1) * 0.5);
c1 = round(size(C, 2) * 0.15);
c2 = round(size(C, 2) * 0.85);
band = double(squeeze(C(row, c1:c2, :)));
distinctColors = size(unique(band, 'rows'), 1);
% Interpolated: hundreds of shades. Flat regression: only a few dozen. The gap
% is wide, so a conservative threshold cleanly rejects the flat failure mode.
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
