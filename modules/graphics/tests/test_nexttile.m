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
t = tiledlayout(2, 2);
ax1 = nexttile;
assert_istrue(isgraphics(ax1, 'axes'));
ax2 = nexttile;
assert_istrue(isgraphics(ax2, 'axes'));
assert_isfalse(ax1 == ax2);
close all;
%=============================================================================
t = tiledlayout(2, 2);
nexttile;
plot(1:10, sin(1:10));
t.TileSpacing = 'compact';
t.Padding = 'compact';
assert_isequal(t.TileSpacing, 'compact');
assert_isequal(t.Padding, 'compact');
axCurrent = gca();
posBefore = axCurrent.OuterPosition;
topBefore = posBefore(2) + posBefore(4);
t.Subtitle.String = 'An Insightful Subtitle';
t.Subtitle.FontAngle = 'italic';
posAfter = axCurrent.OuterPosition;
topAfter = posAfter(2) + posAfter(4);
assert_isequal(t.Subtitle.String, 'An Insightful Subtitle');
assert_isequal(t.Subtitle.FontAngle, 'italic');
assert_istrue(topAfter < topBefore);
close all;
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile;
ax3 = nexttile([1 2]);
assert_istrue(isgraphics(ax3, 'axes'));
close all;
%=============================================================================
t = tiledlayout(2, 3);
nexttile;
nexttile;
ax_ref = nexttile(1);
assert_istrue(isgraphics(ax_ref, 'axes'));
close all;
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile([2 1]);
ax1b = nexttile(1);
assert_isequal(ax1, ax1b);
ax3 = nexttile(3);
assert_isfalse(ax1 == ax3);
assert_isequal(tilenum(ax3), 3);
close all;
%=============================================================================
t = tiledlayout(2, 2);
ax1 = nexttile(1, [1 2]);
ax1b = nexttile(1, [1 2]);
assert_isequal(ax1, ax1b);
ax2 = nexttile(2);
assert_isfalse(ax1 == ax2);
assert_isequal(tilenum(ax2), 2);
close all;
%=============================================================================
t = tiledlayout('vertical');
ax1 = nexttile;
ax2 = nexttile;
ax3 = nexttile;
assert_isequal(t.GridSize, [3, 1]);
assert_isequal(tilenum(ax1), 1);
assert_isequal(tilenum(ax3), 3);
close all;
%=============================================================================
t = tiledlayout('horizontal');
ax1 = nexttile;
ax2 = nexttile;
ax3 = nexttile;
assert_isequal(t.GridSize, [1, 3]);
assert_isequal(tilenum(ax2), 2);
assert_isequal(tilenum(ax3), 3);
close all;
%=============================================================================
t = tiledlayout(2, 2);
east = nexttile('east');
east2 = nexttile('EAST');
assert_isequal(east, east2);
assert_istrue(isnan(tilenum(east)));
close all;
%=============================================================================
t = tiledlayout(1, 1);
nexttile;
assert_checkerror('nexttile', _('No empty tile is available in the fixed tiled layout.'));
close all;
%=============================================================================
t1 = tiledlayout(1, 2);
ax1 = nexttile(t1);
t2 = tiledlayout('flow');
ax2 = nexttile(t2);
assert_isequal(tilenum(ax1), 1);
assert_isequal(tilenum(ax2), 1);
assert_isfalse(ax1 == ax2);
close all;
%=============================================================================
