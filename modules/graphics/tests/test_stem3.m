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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
f = figure('Visible', 'off');
Z = [1 2 3; 4 5 6];
h = stem3(Z);
asserts.istrue(isgraphics(h, 'stem'));
asserts.isequal(h.Type, 'stem');
asserts.isequal(numel(h.Children), 0);
asserts.isequal(h.XData, [1 1 2 2 3 3]);
asserts.isequal(h.YData, [1 2 1 2 1 2]);
asserts.isequal(h.ZData, [1 4 2 5 3 6]);
asserts.isequal(h.XDataMode, 'manual');
asserts.isequal(h.YDataMode, 'manual');
asserts.isequal(h.ZDataMode, 'manual');
asserts.isequal(h.MarkerEdgeColor, 'auto');
asserts.isequal(h.MarkerFaceColor, 'none');
asserts.isequal(h.UserData, []);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XLim, [1 3]);
asserts.isequal(ax.XTick, [1 1.5 2 2.5 3]);
renderer = h.RendererState.Renderer;
asserts.istrue(isgraphics(renderer, 'hggroup'));
asserts.isequal(numel(renderer.Children), 2);
stems = renderer.Children(end);
markers = renderer.Children(end - 1);
asserts.istrue(isgraphics(stems, 'line'));
asserts.istrue(isgraphics(markers, 'line'));
asserts.isequal(markers.XData, [1 1 2 2 3 3]);
asserts.isequal(markers.YData, [1 2 1 2 1 2]);
asserts.isequal(markers.ZData, [1 4 2 5 3 6]);
asserts.isequal(ax.View, [-37.5 30]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
x = [1 2 3];
y = [4 5 6];
z = [7 8 9];
h = stem3(ax, x, y, z, 'r--', 'filled', 'LineWidth', 2);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.Type, 'stem');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.MarkerFaceColor, 'auto');
asserts.isequal(h.MarkerEdgeColor, 'auto');
asserts.isequal(h.XData, x(:)');
asserts.isequal(h.YData, y(:)');
asserts.isequal(h.ZData, z(:)');
asserts.isequal(h.XDataMode, 'manual');
asserts.isequal(h.YDataMode, 'manual');
asserts.isequal(h.ZDataMode, 'manual');
renderer = h.RendererState.Renderer;
asserts.isequal(renderer.Children(end).LineStyle, '--');
asserts.isequal(renderer.Children(end).LineWidth, 2);
asserts.isequal(renderer.Children(end - 1).Marker, 'o');
asserts.isequal(renderer.Children(end - 1).MarkerFaceColor, [1 0 0]);
asserts.isequal(renderer.Children(end - 1).XData, x(:)');
asserts.isequal(renderer.Children(end - 1).YData, y(:)');
asserts.isequal(renderer.Children(end - 1).ZData, z(:)');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
[x, y] = meshgrid(1:3, 1:2);
z = x + y;
h = stem3(ax, x, y, z, 'Marker', 's');
asserts.isequal(h.Marker, 'square');
asserts.isequal(h.RendererState.Renderer.Children(end - 1).Marker, 'square');
close(f);
%=============================================================================
try
  stem3({1, 2});
  error('Nelson:test:noErrorRaised', 'Expected error was not raised.');
catch ME
  asserts.isequal(ME.identifier, 'Nelson:validators:mustBeNumeric');
end
try
  stem3(1:2, 1:3, 1:2);
  error('Nelson:test:noErrorRaised', 'Expected error was not raised.');
catch ME
  asserts.isequal(ME.identifier, 'Nelson:dimagree:xYZSizeX');
end
%=============================================================================
