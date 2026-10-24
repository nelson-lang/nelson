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
f = figure('Visible', 'off');
verts = {[0 0; 0.5 0.2; 1 0.5; 1.5 0.8]};
h = streamparticles(verts);
asserts.isequal(h(1).LineStyle, 'none');
asserts.isequal(h(1).Marker, 'o');
asserts.isequal(h(1).MarkerFaceColor, [1 0 0]);
asserts.isequal(h(1).XData, [0 0.5 1 1.5]);
asserts.isequal(h(1).YData, [0 0.2 0.5 0.8]);
asserts.isequal(h(1).ZData, [0 0 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = streamparticles(verts, 4, 'Animate', 0, 'MarkerFaceColor', 'red');
asserts.isequal(numel(h), 1);
asserts.istrue(isgraphics(h(1), 'line'));
asserts.isequal(h(1).LineStyle, 'none');
asserts.isequal(h(1).Marker, 'o');
asserts.isequal(h(1).MarkerFaceColor, [1 0 0]);
asserts.isapprox(h(1).Color, h(1).Parent.ColorOrder(1, :), 1e-12);
asserts.isequal(h(1).XData, [0 0.5 1 1.5]);
asserts.isequal(h(1).YData, [0 0.2 0.5 0.8]);
asserts.isequal(h(1).ZData, [0 0 0 0]);
ax = gca();
asserts.isapprox(ax.XLim, [0 1.5], 1e-12);
asserts.isapprox(ax.YLim, [0 0.8], 1e-12);
asserts.isapprox(ax.ZLim, [-1 1], 1e-12);
asserts.isapprox(ax.DataAspectRatio, [1.875 1 2.5], 1e-12);
asserts.isapprox(ax.XTick, [0 0.5 1 1.5], 1e-12);
asserts.isapprox(ax.YTick, 0:0.1:0.8, 1e-12);
asserts.isequal(h(1).UserData.ChartType, 'streamparticles');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = streamparticles(ax, {[0 0 0; 1 0 1; 2 1 2]}, 2, 'Marker', '+');
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).Marker, '+');
asserts.isequal(numel(h(1).XData), 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = streamparticles({[0 0; 1 1]}, 1, 'Color', [0.2 0.3 0.4]);
asserts.isapprox(h.Color, [0.2 0.3 0.4], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h0 = line(NaN, NaN);
h = streamparticles(h0, {[0 0; 1 0; 2 0; 3 0], [0 1; 1 1; 2 1; 3 1]}, 2, ...
  'ParticleAlignment', 'on');
asserts.isequal(h, h0);
asserts.isequal(h.XData, [0 2 0 2]);
asserts.isequal(h.YData, [0 0 1 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
ax.XLim = [-10 10];
ax.YLim = [-20 20];
h = streamparticles(ax, {[2 3]}, 1);
asserts.isequal(h.XData, 2);
asserts.isequal(h.YData, 3);
asserts.isapprox(ax.XLim, [-10 10], 1e-12);
asserts.isapprox(ax.YLim, [-20 20], 1e-12);
asserts.isapprox(ax.ZLim, [-1 1], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = streamparticles({[2 3]}, 1);
ax = gca();
asserts.isequal(h.XData, 2);
asserts.isequal(h.YData, 3);
asserts.isapprox(ax.XLim, [1 3], 1e-12);
asserts.isapprox(ax.YLim, [2 4], 1e-12);
asserts.isapprox(ax.ZLim, [-1 1], 1e-12);
close(f);
%=============================================================================
asserts.checkerror('streamparticles(1, 2, 3, 4, 5);', getString(message('Nelson:graphics:invalidStreamlineVertices', 'Streamline vertices')));
asserts.checkerror('streamparticles({[1 2 3 4]});', getString(message('Nelson:streamparticles:verticesNotNumericArrays')));
%=============================================================================
