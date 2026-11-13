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
% Legend and colorbar FontSize is 0.9 x the axes FontSize in auto mode.
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 500 400]);
ax = axes(f);
plot(ax, 1:3);
l = legend(ax, 'a');
c = colorbar(ax);
drawnow();
assert_isequal(ax.FontSize, 10);
assert_isapprox(l.FontSize, 9, 1e-12);
assert_isequal(l.FontSizeMode, 'auto');
assert_isapprox(c.FontSize, 9, 1e-12);
assert_isequal(c.FontSizeMode, 'auto');
%=============================================================================
% Axes FontSize set: both follow, without drawnow too.
ax.FontSize = 12;
assert_isapprox(l.FontSize, 10.8, 1e-12);
assert_isapprox(c.FontSize, 10.8, 1e-12);
drawnow();
assert_isapprox(l.FontSize, 10.8, 1e-12);
assert_isequal(l.FontSizeMode, 'auto');
assert_isapprox(c.FontSize, 10.8, 1e-12);
%=============================================================================
% Axes back to auto, then shrunk by a resize, then grown again.
ax.FontSizeMode = 'auto';
drawnow();
assert_isapprox(l.FontSize, 9, 1e-12);
f.Position = [100 100 160 140];
drawnow();
assert_isequal(ax.FontSize, 8);
assert_isapprox(l.FontSize, 7.2, 1e-12);
assert_isapprox(c.FontSize, 7.2, 1e-12);
f.Position = [100 100 500 400];
drawnow();
assert_isequal(ax.FontSize, 10);
assert_isapprox(l.FontSize, 9, 1e-12);
assert_isapprox(c.FontSize, 9, 1e-12);
%=============================================================================
% A FontSize set on the legend switches it to manual; FontSizeMode auto restores it.
l.FontSize = 11;
drawnow();
assert_isequal(l.FontSizeMode, 'manual');
ax.FontSize = 14;
drawnow();
assert_isequal(l.FontSize, 11);
assert_isapprox(c.FontSize, 12.6, 1e-12);
l.FontSizeMode = 'auto';
drawnow();
assert_isapprox(l.FontSize, 12.6, 1e-12);
c.FontSize = 5;
ax.FontSize = 20;
drawnow();
assert_isequal(c.FontSize, 5);
assert_isequal(c.FontSizeMode, 'manual');
set(c, 'FontSizeMode', 'auto');
drawnow();
assert_isapprox(c.FontSize, 18, 1e-12);
assert_isequal(get(c, 'FontSizeMode'), 'auto');
close(f);
%=============================================================================
% FontSize given at creation is manual.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:3);
l = legend(ax, 'a', 'FontSize', 13);
c = colorbar(ax, 'FontSize', 6);
drawnow();
assert_isequal(l.FontSizeMode, 'manual');
assert_isequal(c.FontSizeMode, 'manual');
ax.FontSize = 20;
drawnow();
assert_isequal(l.FontSize, 13);
assert_isequal(c.FontSize, 6);
close(f);
%=============================================================================
% Created after the axes FontSize changed: legend title and colorbar label follow.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:3);
ax.FontSize = 16;
l = legend(ax, 'a');
assert_isapprox(l.FontSize, 14.4, 1e-12);
set(l.Title, 'String', 'T');
c = colorbar(ax);
set(c.Label, 'String', 'L');
drawnow();
assert_isapprox(l.Title.FontSize, 14.4, 1e-12);
assert_isapprox(c.Label.FontSize, 14.4, 1e-12);
ax.FontSize = 11.3;
drawnow();
assert_isapprox(l.FontSize, 10.17, 1e-12);
assert_isapprox(c.FontSize, 10.17, 1e-12);
close(f);
%=============================================================================
% Bubble legend follows the same rule.
f = figure('Visible', 'off');
ax = axes(f);
bubblechart(ax, 1:3, 1:3, [1 5 9]);
b = bubblelegend(ax);
drawnow();
assert_isapprox(b.FontSize, 9, 1e-12);
assert_isequal(b.FontSizeMode, 'auto');
ax.FontSize = 14;
drawnow();
assert_isapprox(b.FontSize, 12.6, 1e-12);
b.FontSize = 7;
ax.FontSize = 10;
drawnow();
assert_isequal(b.FontSize, 7);
assert_isequal(b.FontSizeMode, 'manual');
close(f);
%=============================================================================
% A resize keeps the axes PositionConstraint: an outside legend does not shrink the font.
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 480 420]);
ax = axes(f);
plot(ax, 1:10);
drawnow();
f.Position = [100 100 300 300];
drawnow();
assert_isequal(ax.PositionConstraint, 'outerposition');
assert_isequal(ax.FontSize, 9.5);
l = legend(ax, 'G1', 'Location', 'northeastoutside');
drawnow();
assert_isequal(ax.FontSize, 9.5);
assert_isapprox(l.FontSize, 8.55, 1e-12);
c = colorbar(ax);
delete(l);
drawnow();
assert_isequal(ax.FontSize, 9.5);
assert_isapprox(c.FontSize, 8.55, 1e-12);
close(f);
%=============================================================================
% Tiled layout axes shrink to 9: their colorbars follow.
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 560 420]);
t = tiledlayout(f, 2, 2);
a = nexttile(t);
surf(a, peaks(10));
c = colorbar(a);
drawnow();
assert_isequal(a.FontSize, 9);
assert_isapprox(c.FontSize, 8.1, 1e-12);
close(f);
%=============================================================================
