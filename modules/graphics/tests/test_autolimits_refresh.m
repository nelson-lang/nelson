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
h2 = line(ax, [0 2], [0 50]);
ylim(ax, 'auto');
assert_isapprox(ax.YLim, [0 50], 1e-9);
%=============================================================================
% a child property change refreshes the automatic limits
h2.AffectAutoLimits = 'off';
assert_isapprox(ax.YLim, [0 1], 1e-9);
assert_isequal(ax.YLimMode, 'auto');
h2.AffectAutoLimits = 'on';
assert_isapprox(ax.YLim, [0 50], 1e-9);
h2.YData = [0 5];
assert_isapprox(ax.YLim, [0 5], 1e-9);
set(h2, 'YData', [0 80]);
assert_isapprox(ax.YLim, [0 80], 1e-9);
h2.AffectAutoLimits = 'off';
h2.YData = [0 90];
assert_isapprox(ax.YLim, [0 1], 1e-9);
h2.AffectAutoLimits = 'on';
assert_isapprox(ax.YLim, [0 90], 1e-9);
%=============================================================================
% requesting 'auto' on an automatic limit recomputes it
h2.AffectAutoLimits = 'off';
ylim(ax, 'auto');
assert_isapprox(ax.YLim, [0 1], 1e-9);
xlim(ax, 'auto');
assert_isapprox(ax.XLim, [0 1], 1e-9);
axis(ax, 'auto');
assert_isapprox(ax.YLim, [0 1], 1e-9);
h2.AffectAutoLimits = 'on';
ylim(ax, 'auto');
assert_isapprox(ax.YLim, [0 90], 1e-9);
%=============================================================================
% a manual limit is left alone by child changes
ylim(ax, [0 100]);
assert_isequal(ax.YLimMode, 'manual');
h2.YData = [0 5];
h2.AffectAutoLimits = 'off';
assert_isapprox(ax.YLim, [0 100], 1e-9);
assert_isequal(ax.YLimMode, 'manual');
ylim(ax, 'auto');
assert_isapprox(ax.YLim, [0 1], 1e-9);
%=============================================================================
% the front end snapshot carries the refreshed limits
h2.AffectAutoLimits = 'on';
snapshot = __web_figure_snapshot__(f);
assert_isapprox(snapshot.axes{1}.YLim, [0 5], 1e-9);
h2.AffectAutoLimits = 'off';
snapshot = __web_figure_snapshot__(f);
assert_isapprox(snapshot.axes{1}.YLim, [0 1], 1e-9);
%=============================================================================
close(f);
%=============================================================================
