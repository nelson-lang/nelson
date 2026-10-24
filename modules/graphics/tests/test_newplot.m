%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
h = newplot();
asserts.isequal(h.Type, 'axes');
%=============================================================================
close('all');
h = newplot([]);
asserts.isequal(h.Type, 'axes');
asserts.isequal(ancestor(h, 'figure'), groot().CurrentFigure);
%=============================================================================
f1 = figure();
ax1 = axes(f1);
f2 = figure();
axes(f2);
h = newplot(ax1);
asserts.isequal(h, ax1);
asserts.isequal(ancestor(h, 'figure'), f1);
close(f1);
close(f2);
%=============================================================================
% newplot on a NextPlot='replace' axes restores the auto limit modes.
f = figure('Visible', 'off');
ax = axes(f);
ax.XLim = [2 4];
ax.YLim = [3 6];
ax.NextPlot = 'replace';
newplot(ax);
asserts.isequal(ax.XLimMode, 'auto');
asserts.isequal(ax.YLimMode, 'auto');
close(f);
%=============================================================================
% newplot on a NextPlot='replacechildren' axes keeps the manual limits/modes.
f = figure('Visible', 'off');
ax = axes(f);
ax.XLim = [2 4];
ax.YLim = [3 6];
ax.NextPlot = 'replacechildren';
newplot(ax);
asserts.isequal(ax.XLim, [2 4]);
asserts.isequal(ax.YLim, [3 6]);
asserts.isequal(ax.XLimMode, 'manual');
asserts.isequal(ax.YLimMode, 'manual');
close(f);
%=============================================================================
% End-to-end: a NextPlot='replace' axes (the default) restores the auto limit
% modes on the next plot, so a leftover 'axis manual' does not freeze the limits.
f = figure('Visible', 'off');
axis manual
plot(1:5, 1:5);
asserts.isequal(gca().XLimMode, 'auto');
asserts.isequal(gca().YLimMode, 'auto');
asserts.isequal(gca().XLim, [1 5]);
close(f);
%=============================================================================
% End-to-end: NextPlot='replacechildren' keeps the manual modes (limits stay
% frozen), unlike 'replace'.
f = figure('Visible', 'off');
ax = axes();
ax.NextPlot = 'replacechildren';
axis manual
plot(1:5, 1:5);
asserts.isequal(gca().XLimMode, 'manual');
asserts.isequal(gca().XLim, [0 1]);
close(f);
%=============================================================================
