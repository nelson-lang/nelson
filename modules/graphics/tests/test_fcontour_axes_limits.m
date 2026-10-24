%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% A function contour frames the region it was asked to sample, not the region
% its level lines happen to occupy. The limits therefore follow XRange/YRange
% whether the function draws many lines, a few small ones, or none at all.
% Every expected value below was measured.
%=============================================================================
% A constant function carries no level line to draw, and still keeps the range.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) 0 * x + 0 * y + 1);
asserts.isequal(size(h.ContourMatrix), [2 0]);
asserts.isequal(ax.XLim, [-5 5]);
asserts.isequal(ax.YLim, [-5 5]);
asserts.isequal(ax.XLimMode, 'auto');
asserts.isequal(ax.YLimMode, 'auto');
close(f);
%=============================================================================
% Drawing nothing is not worth a warning here: the range still frames the plot.
f = figure('Visible', 'off');
ax = axes('Parent', f);
lastwarn('', '');
h = fcontour(ax, @(x, y) 0 * x + 0 * y + 1);
[msg, id] = lastwarn();
asserts.isequal(msg, '');
asserts.isequal(id, '');
close(f);
%=============================================================================
% The same holds when the function has no finite value at all.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) 0 * x + 0 * y + NaN);
asserts.isequal(size(h.ContourMatrix), [2 0]);
asserts.isequal(ax.XLim, [-5 5]);
asserts.isequal(ax.YLim, [-5 5]);
close(f);
%=============================================================================
% An explicit range is kept just as well when nothing is drawn inside it.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) 0 * x + 0 * y + 1, [0 3 -2 2]);
asserts.isequal(h.XRange, [0 3]);
asserts.isequal(h.YRange, [-2 2]);
asserts.isequal(ax.XLim, [0 3]);
asserts.isequal(ax.YLim, [-2 2]);
close(f);
%=============================================================================
% A function that does draw, but whose lines sit in a small disc, frames the
% whole range too: the lines never dictate the limits.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) exp(-(x .^ 2 + y .^ 2)));
asserts.istrue(size(h.ContourMatrix, 2) > 0);
asserts.isequal(ax.XLim, [-5 5]);
asserts.isequal(ax.YLim, [-5 5]);
close(f);
%=============================================================================
% The limits report the range verbatim, drawn or not, without rounding it.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) x .^ 2 + y .^ 2, [0.37 2.13 -1.77 3.05]);
asserts.isapprox(ax.XLim, [0.37 2.13], 1e-12);
asserts.isapprox(ax.YLim, [-1.77 3.05], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) 0 * x + 0 * y + 1, [0.37 2.13 -1.77 3.05]);
asserts.isapprox(ax.XLim, [0.37 2.13], 1e-12);
asserts.isapprox(ax.YLim, [-1.77 3.05], 1e-12);
close(f);
%=============================================================================
% Held on top of an existing plot, the range widens the limits instead of
% replacing them: the limits stay automatic and cover both.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, [10 20], [30 40]);
hold(ax, 'on');
h = fcontour(ax, @(x, y) 0 * x + 0 * y + 1);
asserts.isequal(ax.XLim, [-5 20]);
asserts.isequal(ax.YLim, [-5 40]);
asserts.isequal(ax.XLimMode, 'auto');
close(f);
%=============================================================================
% Several functions at once, none of them drawing anything.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, {@(x, y) 0 * x + 0 * y + 1, @(x, y) 0 * x + 0 * y + 7});
asserts.isequal(size(h), [2 1]);
asserts.isequal(ax.XLim, [-5 5]);
asserts.isequal(ax.YLim, [-5 5]);
close(f);
%=============================================================================
% A drawing function next to a silent one keeps the shared range.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, {@(x, y) 0 * x + 0 * y + 1, @(x, y) x .^ 2 + y .^ 2}, [0 3 -2 2]);
asserts.isequal(ax.XLim, [0 3]);
asserts.isequal(ax.YLim, [-2 2]);
close(f);
%=============================================================================
% Two function contours with their own ranges span both ranges.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h1 = fcontour(ax, @(x, y) 0 * x + 0 * y + 1, [0 3 -2 2]);
hold(ax, 'on');
h2 = fcontour(ax, @(x, y) 0 * x + 0 * y + 2, [-8 -1 4 9]);
asserts.isequal(ax.XLim, [-8 3]);
asserts.isequal(ax.YLim, [-2 9]);
close(f);
%=============================================================================
% Filling the contour changes nothing to the framing.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) 0 * x + 0 * y + 1, 'Fill', 'on');
asserts.isequal(ax.XLim, [-5 5]);
asserts.isequal(ax.YLim, [-5 5]);
close(f);
%=============================================================================
% An implicit function line samples a range the same way, but frames the
% solution set it drew rather than that range: sampling over [-5 5] keeps the
% limits on the circle of radius 2. It must not follow the rule above.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fimplicit(ax, @(x, y) x .^ 2 + y .^ 2 - 4);
asserts.isequal(ax.XLim, [-2 2]);
asserts.isequal(ax.YLim, [-2 2]);
close(f);
%=============================================================================
