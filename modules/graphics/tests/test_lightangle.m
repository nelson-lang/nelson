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
close all
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = lightangle(ax, 30, 45);
asserts.istrue(isgraphics(h, 'light'));
asserts.isequal(h.Style, 'infinite');
oldPosition = h.Position;
lightangle(h, 60, 20);
asserts.isfalse(isequal(h.Position, oldPosition));
close(f);
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = lightangle(ax, 45, 30);
asserts.isapprox(h.Position, [0.866025403784439 -0.866025403784439 0.707106781186547], 1e-12);
close(f);
f = figure('Visible', 'off');
f.Position = [100 100 700 520];
ax = axes('Parent', f);
surf(ax, peaks(30));
h = lightangle(ax, 45, 30);
asserts.isapprox(h.Position, [1.10397010829098 -1.10397010829098 0.600925212577331], 1e-12);
view(ax, 35, 28);
drawnow();
asserts.isequal(ax.ZLim, [-10 10]);
asserts.isapprox(ax.DataAspectRatio, [1.5 1.5 1], 1e-12);
asserts.isapprox(ax.PlotBoxAspectRatio, [1 0.920246455468343 0.836386047732169], 2e-3);
asserts.isequal(ax.XTick, 0:5:30);
asserts.isequal(ax.YTick, 0:10:30);
asserts.isequal(ax.ZTick, -10:5:10);
view(ax, -37.5, 30);
drawnow();
asserts.isequal(ax.ZLim, [-10 10]);
asserts.isapprox(ax.DataAspectRatio, [1.5 1.5 1], 1e-12);
asserts.isapprox(ax.PlotBoxAspectRatio, [1 0.938686276468375 0.844309443386719], 2e-3);
asserts.isequal(ax.XTick, 0:5:30);
asserts.isequal(ax.YTick, 0:10:30);
asserts.isequal(ax.ZTick, -10:5:10);
close all
%=============================================================================
