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
% bar keeps automatic limits; only a new plot with few integer positions fixes the position ticks.
f = figure('Visible', 'off');
ax = axes(f);
bar(ax, 1:10, (1:10) * 10);
drawnow();
assert_isequal(ax.XLimMode, 'auto');
assert_isequal(ax.YLimMode, 'auto');
assert_isequal(ax.XTickMode, 'manual');
assert_isequal(ax.YTickMode, 'auto');
assert_isapprox(ax.XLim, [-0.2 11.2], 1e-12);
assert_isapprox(ax.YLim, [0 100], 1e-12);
assert_isequal(ax.XTick, 1:10);
%=============================================================================
% Grouped bars: group width is min(0.8, n / (n + 1.5)), whatever the bar width.
cla(ax, 'reset');
bar(ax, [1 2 3; 4 5 6; 7 8 9]);
drawnow();
assert_isapprox(ax.XLim, [1 - 22 / 45, 3 + 22 / 45], 1e-12);
assert_isapprox(ax.YLim, [0 9], 1e-12);
cla(ax, 'reset');
bar(ax, [1 2 3; 4 5 6; 7 8 9], 0.5);
drawnow();
assert_isapprox(ax.XLim, [1 - 41 / 90, 3 + 41 / 90], 1e-12);
cla(ax, 'reset');
bar(ax, [1 2 3; 4 5 6]');
drawnow();
assert_isapprox(ax.XLim, [1 - 17 / 35, 3 + 17 / 35], 1e-12);
%=============================================================================
cla(ax, 'reset');
bar(ax, [1 2 3; 4 5 6; 7 8 9], 'stacked');
drawnow();
assert_isapprox(ax.XLim, [-0.2 4.2], 1e-12);
assert_isapprox(ax.YLim, [0 25], 1e-12);
%=============================================================================
cla(ax, 'reset');
bar(ax, [3 -2 5 -7]);
drawnow();
assert_isapprox(ax.YLim, [-8 6], 1e-12);
%=============================================================================
% The value range starts at BaseValue, not at zero.
cla(ax, 'reset');
bar(ax, [3 2 5], 'BaseValue', 1);
drawnow();
assert_isapprox(ax.YLim, [1 5], 1e-12);
%=============================================================================
cla(ax, 'reset');
barh(ax, [3 2 5 7]);
drawnow();
assert_isequal(ax.XLimMode, 'auto');
assert_isequal(ax.YLimMode, 'auto');
assert_isequal(ax.YTickMode, 'manual');
assert_isequal(ax.XTickMode, 'auto');
assert_isapprox(ax.XLim, [0 7], 1e-12);
assert_isapprox(ax.YLim, [-0.2 5.2], 1e-12);
%=============================================================================
% More than 15 positions or held axes keep automatic ticks.
cla(ax, 'reset');
bar(ax, 1:30, 1:30);
drawnow();
assert_isequal(ax.XTickMode, 'auto');
cla(ax, 'reset');
plot(ax, 0:20, (0:20) * 2);
hold(ax, 'on');
bar(ax, 1:5, [3 2 5 4 1]);
hold(ax, 'off');
drawnow();
assert_isequal(ax.XTickMode, 'auto');
assert_isapprox(ax.XLim, [-0.2 20], 1e-12);
assert_isapprox(ax.YLim, [0 40], 1e-12);
cla(ax, 'reset');
bar(ax, 1:5, [3 2 5 4 1]);
hold(ax, 'on');
bar(ax, 6:8, [9 9 9]);
hold(ax, 'off');
drawnow();
assert_isequal(ax.XTick, 1:5);
assert_isapprox(ax.XLim, [-0.2 9.2], 1e-12);
assert_isapprox(ax.YLim, [0 9], 1e-12);
%=============================================================================
% A manual X range refits the auto Y limits to the visible bars.
cla(ax, 'reset');
bar(ax, [1 2 3; 4 5 6; 7 8 9]);
ax.XLim = [1.5 2.5];
drawnow();
assert_isequal(ax.YLimMode, 'auto');
assert_isapprox(ax.YLim, [0 6], 1e-12);
close(f);
%=============================================================================
