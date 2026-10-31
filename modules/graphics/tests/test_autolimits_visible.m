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
f = figure('Visible', 'off');
ax = axes('Parent', f);
h1 = line(ax, [0 1], [0 1]);
h2 = line(ax, [0 2], [0 80]);
assert_isapprox(ax.XLim, [0 2], 1e-9);
assert_isapprox(ax.YLim, [0 80], 1e-9);
%=============================================================================
% an invisible child is left out of the automatic limits
h2.Visible = 'off';
assert_isapprox(ax.XLim, [0 1], 1e-9);
assert_isapprox(ax.YLim, [0 1], 1e-9);
assert_isequal(ax.YLimMode, 'auto');
h2.Visible = 'on';
assert_isapprox(ax.XLim, [0 2], 1e-9);
assert_isapprox(ax.YLim, [0 80], 1e-9);
set(h2, 'Visible', 'off');
assert_isapprox(ax.YLim, [0 1], 1e-9);
set(h2, 'Visible', 'on');
assert_isapprox(ax.YLim, [0 80], 1e-9);
%=============================================================================
% every child invisible: the limits fall back to the default
h1.Visible = 'off';
h2.Visible = 'off';
assert_isapprox(ax.XLim, [0 1], 1e-9);
assert_isapprox(ax.YLim, [0 1], 1e-9);
assert_isapprox(ax.ZLim, [0 1], 1e-9);
assert_isequal(ax.XLimMode, 'auto');
h1.Visible = 'on';
h2.Visible = 'on';
assert_isapprox(ax.YLim, [0 80], 1e-9);
%=============================================================================
% a child created invisible does not count until it is shown
h3 = line(ax, [0 1], [0 900], 'Visible', 'off');
assert_isapprox(ax.YLim, [0 80], 1e-9);
h3.Visible = 'on';
assert_isapprox(ax.YLim, [0 900], 1e-9);
delete(h3);
%=============================================================================
% hggroup: the group Visible or the Visible of one of its children
g = hggroup('Parent', ax);
h4 = line([0 1], [0 500], 'Parent', g);
assert_isapprox(ax.YLim, [0 500], 1e-9);
g.Visible = 'off';
assert_isapprox(ax.YLim, [0 80], 1e-9);
g.Visible = 'on';
assert_isapprox(ax.YLim, [0 500], 1e-9);
h4.Visible = 'off';
assert_isapprox(ax.YLim, [0 80], 1e-9);
h4.Visible = 'on';
assert_isapprox(ax.YLim, [0 500], 1e-9);
delete(g);
%=============================================================================
% Z limits
h5 = line(ax, [0 1], [0 1], [0 10]);
h6 = line(ax, [0 1], [0 1], [0 100]);
assert_isapprox(ax.ZLim, [0 100], 1e-9);
h6.Visible = 'off';
assert_isapprox(ax.ZLim, [0 10], 1e-9);
h6.Visible = 'on';
assert_isapprox(ax.ZLim, [0 100], 1e-9);
delete(h5);
delete(h6);
%=============================================================================
% a manual limit is left alone
ylim(ax, [0 5]);
h2.Visible = 'off';
h2.Visible = 'on';
assert_isapprox(ax.YLim, [0 5], 1e-9);
assert_isequal(ax.YLimMode, 'manual');
ylim(ax, 'auto');
assert_isapprox(ax.YLim, [0 80], 1e-9);
h2.Visible = 'off';
assert_isapprox(ax.YLim, [0 1], 1e-9);
%=============================================================================
% the front end snapshot carries the refreshed limits
snapshot = __web_figure_snapshot__(f);
assert_isapprox(snapshot.axes{1}.YLim, [0 1], 1e-9);
h2.Visible = 'on';
snapshot = __web_figure_snapshot__(f);
assert_isapprox(snapshot.axes{1}.YLim, [0 80], 1e-9);
close(f);
%=============================================================================
% color limits follow the visible color-mapped children
f = figure('Visible', 'off');
ax = axes('Parent', f);
p = patch('Parent', ax, 'Faces', [1 2 3], 'Vertices', [0 0; 1 0; 1 300], ...
    'FaceVertexCData', [1; 5; 9], 'FaceColor', 'interp');
assert_isapprox(ax.YLim, [0 300], 1e-9);
assert_isapprox(ax.CLim(2), 9, 1e-9);
p.Visible = 'off';
assert_isapprox(ax.YLim, [0 1], 1e-9);
assert_isapprox(ax.CLim, [0 1], 1e-9);
p.Visible = 'on';
assert_isapprox(ax.YLim, [0 300], 1e-9);
assert_isapprox(ax.CLim(2), 9, 1e-9);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
s = surface(ax, peaks(10) * 10);
l = line(ax, [1 10], [1 10], [0 5]);
clim = ax.CLim;
zlim = ax.ZLim;
assert_istrue(zlim(1) < -50);
s.Visible = 'off';
assert_isapprox(ax.ZLim, [0 5], 1e-9);
assert_isapprox(ax.CLim, [0 1], 1e-9);
s.Visible = 'on';
assert_isapprox(ax.ZLim, zlim, 1e-9);
assert_isapprox(ax.CLim, clim, 1e-9);
close(f);
%=============================================================================
