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
% The marginal (top and right) histograms must render in the same colour as
% the scatter markers. The SVG export rounds marker colours but truncates
% stroke colours, so without quantization the marginal outlines render one
% 8-bit level darker than the scatter points of the same series colour.
%=============================================================================
close all
rng(7);
x = randn(200, 1);
y = 0.5 * x + randn(200, 1);
f = figure('Visible', 'off');
scatterhistogram(f, x, y, 'NumBins', 20);
drawnow();
svgFile = [tempname(), '.svg'];
saveas(f, svgFile);
close(f);
svgText = fileread(svgFile);
rmfile(svgFile);
%=============================================================================
% Scatter markers are emitted as <circle> elements, the marginal histograms as
% <polyline> stairs. Their colours must be identical.
markerColors = regexp(svgText, '<circle[^>]*fill="(rgba\([^"]+)"', 'tokens');
strokeColors = regexp(svgText, '<polyline[^>]*stroke="(rgba\([^"]+)"', 'tokens');
asserts.istrue(~isempty(markerColors));
asserts.istrue(~isempty(strokeColors));
markerColor = markerColors{1}{1};
for k = 1:numel(strokeColors)
  asserts.isequal(strokeColors{k}{1}, markerColor);
end
%=============================================================================
% The public Color property stays the exact series colour; only the internal
% marginal stroke colour is quantized for the export.
h = scatterhistogram(figure('Visible', 'off'), x, y);
asserts.isapprox(h.Color, [0.066 0.443 0.745], 1e-12);
close all
%=============================================================================
