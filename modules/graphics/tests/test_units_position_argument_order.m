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
% properties apply in argument order: a Position given before Units is
% converted when Units changes, a Position given after Units is kept
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
%=============================================================================
p = uipanel(f, 'Position', [10 20 200 100], 'Units', 'pixels');
asserts.isequal(p.Units, 'pixels');
asserts.isapprox(p.Position, [6001 8001 120000 40000], 1e-9);
p = uipanel(f, 'Units', 'pixels', 'Position', [10 20 200 100]);
asserts.isequal(p.Position, [10 20 200 100]);
%=============================================================================
p = uibuttongroup(f, 'Position', [10 20 200 100], 'Units', 'pixels');
asserts.isapprox(p.Position, [6001 8001 120000 40000], 1e-9);
tg = uitabgroup(f, 'Position', [10 20 200 100], 'Units', 'pixels');
asserts.isapprox(tg.Position, [6001 8001 120000 40000], 1e-9);
%=============================================================================
u = uicontrol(f, 'Position', [10 20 200 100], 'Units', 'normalized');
asserts.isapprox(u.Position, [0.015 0.0475 1/3 0.25], 1e-12);
u = uicontrol(f, 'Units', 'normalized', 'Position', [0.1 0.2 0.3 0.4]);
asserts.isapprox(u.Position, [0.1 0.2 0.3 0.4], 1e-12);
u = uicontrol(f, 'Position', [0.1 0.2 0.3 0.4], 'Units', 'normalized');
asserts.isapprox(u.Position, [-0.0015 -0.002 0.0005 0.001], 1e-12);
%=============================================================================
ax = axes(f, 'Position', [10 20 200 100], 'Units', 'pixels');
asserts.isapprox(ax.Position, [6001 8001 120000 40000], 1e-9);
ax = axes(f, 'InnerPosition', [10 20 200 100], 'Units', 'pixels');
asserts.isapprox(ax.Position, [6001 8001 120000 40000], 1e-9);
ax = axes(f, 'Units', 'pixels', 'Position', [10 20 200 100]);
asserts.isequal(ax.Position, [10 20 200 100]);
%=============================================================================
h = uipanel(f);
set(h, 'Position', [10 20 200 100], 'Units', 'pixels');
asserts.isapprox(h.Position, [6001 8001 120000 40000], 1e-9);
h = uipanel(f);
set(h, 'Units', 'pixels', 'Position', [10 20 200 100]);
asserts.isequal(h.Position, [10 20 200 100]);
h = axes(f);
set(h, 'Position', [10 20 200 100], 'Units', 'pixels');
asserts.isapprox(h.Position, [6001 8001 120000 40000], 1e-9);
h = uicontrol(f);
set(h, 'Position', [0.1 0.2 0.3 0.4], 'Units', 'normalized');
asserts.isapprox(h.Position, [-0.0015 -0.002 0.0005 0.001], 1e-12);
h = uicontrol(f);
set(h, 'Units', 'normalized', 'Position', [0.1 0.2 0.3 0.4]);
asserts.isapprox(h.Position, [0.1 0.2 0.3 0.4], 1e-12);
%=============================================================================
% Units set to its current value converts nothing
h = uipanel(f, 'Units', 'pixels');
h.Units = 'normalized';
h.Units = 'normalized';
asserts.isapprox(h.Position, [0 0 1 1], 1e-12);
h = uicontrol(f, 'Units', 'pixels', 'Position', [10 20 30 40]);
set(h, 'Units', 'pixels');
asserts.isequal(h.Position, [10 20 30 40]);
%=============================================================================
ax = axes(f);
plot(ax, 1:3);
l = legend(ax, 'a');
set(l, 'Units', 'normalized', 'Position', [0.1 0.2 0.3 0.1]);
asserts.isapprox(l.Position, [0.1 0.2 0.3 0.1], 1e-12);
set(l, 'Position', [10 20 30 40], 'Units', 'pixels');
asserts.isapprox(l.Position, [6001 8001 18000 16000], 1e-9);
c = colorbar(ax);
set(c, 'Units', 'pixels');
set(c, 'Units', 'normalized', 'Position', [0.1 0.2 0.05 0.5]);
asserts.isapprox(c.Position, [0.1 0.2 0.05 0.5], 1e-12);
close(f);
%=============================================================================
s = get(groot, 'ScreenSize');
if s(3) > 1 && s(4) > 1
  g = figure('Visible', 'off', 'Position', [100 100 600 400], 'Units', 'normalized');
  asserts.isapprox(g.Position, [99 / s(3), 99 / s(4), 600 / s(3), 400 / s(4)], 1e-9);
  close(g);
  g = figure('Visible', 'off', 'Units', 'normalized', 'Position', [0.1 0.1 0.3 0.3]);
  asserts.isapprox(g.Position, [0.1 0.1 0.3 0.3], 1e-9);
  close(g);
end
%=============================================================================
