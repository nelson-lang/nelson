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
ax = axes(f);
plot(ax, [1 2], [9.6 9.9]);
drawnow;
refX = ax.XLim;
refY = ax.YLim;
%=============================================================================
% all-NaN objects do not take part in the automatic limits
ax = axes(f);
plot(ax, [1 2], [9.6 9.9]);
line(ax, [NaN NaN], [NaN NaN]);
drawnow;
assert_isequal(ax.XLim, refX);
assert_isequal(ax.YLim, refY);
%=============================================================================
ax = axes(f);
hold(ax, 'on');
plot(ax, [1 2], [9.6 9.9]);
scatter(ax, [NaN NaN], [NaN NaN]);
stairs(ax, [NaN NaN], [NaN NaN]);
patch(ax, [NaN NaN NaN], [NaN NaN NaN], 'r');
surface(ax, NaN(2), NaN(2), NaN(2));
plot3(ax, [NaN NaN], [NaN NaN], [NaN NaN]);
drawnow;
assert_isequal(ax.XLim, refX);
assert_isequal(ax.YLim, refY);
%=============================================================================
ax = axes(f);
hold(ax, 'on');
patch(ax, [1 2 2 1], [9.6 9.6 9.9 9.9], 'r');
refPatchY = ax.YLim;
line(ax, [NaN NaN], [NaN NaN]);
drawnow;
assert_isequal(ax.YLim, refPatchY);
assert_istrue(ax.YLim(1) > 9);
%=============================================================================
% an axes holding only all-NaN objects keeps the default limits
ax = axes(f);
line(ax, [NaN NaN], [NaN NaN]);
drawnow;
assert_isequal(ax.XLim, [0 1]);
assert_isequal(ax.YLim, [0 1]);
assert_isequal(ax.ZLim, [0 1]);
%=============================================================================
ax = axes(f);
surface(ax, NaN(2), NaN(2), NaN(2));
drawnow;
assert_isequal(ax.XLim, [0 1]);
assert_isequal(ax.YLim, [0 1]);
assert_isequal(ax.ZLim, [0 1]);
%=============================================================================
ax = axes(f);
h = plot(ax, [1 2], [3 4]);
drawnow;
set(h, 'XData', [NaN NaN], 'YData', [NaN NaN]);
drawnow;
assert_isequal(ax.XLim, [0 1]);
assert_isequal(ax.YLim, [0 1]);
%=============================================================================
% a point counts only when all its coordinates are finite
ax = axes(f);
plot(ax, [1 2 30], [5 6 NaN]);
drawnow;
assert_isapprox(ax.XLim, [1 2], 1e-10);
assert_isapprox(ax.YLim, [5 6], 1e-10);
%=============================================================================
ax = axes(f);
plot(ax, [1 2 NaN], [5 6 70]);
drawnow;
assert_isapprox(ax.XLim, [1 2], 1e-10);
assert_isapprox(ax.YLim, [5 6], 1e-10);
%=============================================================================
ax = axes(f);
scatter(ax, [1 2 30], [5 6 NaN]);
drawnow;
assert_isapprox(ax.XLim, [1 2], 1e-10);
assert_isapprox(ax.YLim, [5 6], 1e-10);
%=============================================================================
ax = axes(f);
patch(ax, [1 2 30 NaN], [5 6 NaN 9], 'r');
drawnow;
assert_isapprox(ax.XLim, [1 2], 1e-10);
assert_isapprox(ax.YLim, [5 6], 1e-10);
%=============================================================================
ax = axes(f);
plot3(ax, [1 2 30], [5 6 7], [1 2 NaN]);
drawnow;
assert_isapprox(ax.XLim, [1 2], 1e-10);
assert_isapprox(ax.YLim, [5 6], 1e-10);
assert_isapprox(ax.ZLim, [1 2], 1e-10);
%=============================================================================
ax = axes(f);
surface(ax, [1 2; 1 2], [3 3; 40 4], [5 NaN; 6 7]);
drawnow;
assert_isapprox(ax.XLim, [1 2], 1e-10);
assert_isapprox(ax.ZLim, [5 7], 1e-10);
assert_istrue(ax.YLim(2) >= 40);
%=============================================================================
close(f);
%=============================================================================
