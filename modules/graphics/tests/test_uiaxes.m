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
fig = uifigure('Visible', 'off');
ax = uiaxes(fig);
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(class(ax), 'nelson.ui.control.UIAxes');
assert_isequal(ax.Type, 'axes');
assert_isequal(ax.Units, 'pixels');
assert_isequal(ax.Position, [10 10 400 300]);
assert_isequal(ax.NextPlot, 'replacechildren');
assert_isequal(ax.Parent, fig);
%=============================================================================
% plotting into a uiaxes
% note: requires HandleVisibility 'on' on the figure until targeted plot
% honors axes inside handle-hidden figures
fig.HandleVisibility = 'on';
plot(ax, 1:10, (1:10) .^ 2);
assert_isequal(numel(ax.Children), 1);
%=============================================================================
% name-value overrides
ax2 = uiaxes(fig, 'Position', [50 50 200 150]);
assert_isequal(ax2.Position, [50 50 200 150]);
delete(fig);
%=============================================================================
% UI container parents
fig = uifigure('Visible', 'off');
p = uipanel(fig, 'Position', [10 10 300 220]);
ax = uiaxes(p, 'Position', [20 20 220 150]);
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(ax.Parent, p);
assert_isequal(p.Children, ax);
assert_isequal(ax.Position, [20 20 220 150]);
delete(fig);
%=============================================================================
fig = uifigure('Visible', 'off');
tg = uitabgroup(fig);
t = uitab(tg, 'Title', 'Graph');
ax = uiaxes(t, 'Position', [10 10 100 100]);
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(ax.Parent, t);
assert_isequal(t.Children, ax);
assert_isequal(ax.Position, [10 10 100 100]);
delete(fig);
%=============================================================================
fig = uifigure('Visible', 'on', 'Position', [100 100 760 420]);
fig.HandleVisibility = 'on';
tg = uitabgroup(fig, 'Position', [10 10 740 400]);
t = uitab(tg, 'Title', 'Two Graph');
ax1 = uiaxes(t, 'Position', [20 40 330 300]);
ax2 = uiaxes(t, 'Position', [380 40 330 300]);
plot(ax1, 1:20, 1:20, 'Color', [1 0 0], 'LineWidth', 4);
plot(ax2, 1:20, 20:-1:1, 'Color', [0 0 1], 'LineWidth', 4);
drawnow();
pause(0.2);
frame = getframe(fig);
pixels = double(frame.cdata);
redPixels = pixels(:, :, 1) > 150 & pixels(:, :, 2) < 120 & pixels(:, :, 3) < 120;
bluePixels = pixels(:, :, 1) < 120 & pixels(:, :, 2) < 120 & pixels(:, :, 3) > 150;
assert_istrue(nnz(redPixels) > 25);
assert_istrue(nnz(bluePixels) > 25);
delete(fig);
%=============================================================================
fig = uifigure('Visible', 'off');
bg = uibuttongroup(fig, 'Position', [10 10 300 220]);
ax = uiaxes(bg, 'Position', [20 20 220 150]);
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(ax.Parent, bg);
assert_isequal(bg.Children, ax);
delete(fig);
%=============================================================================
fig = uifigure('Visible', 'off');
g = uigridlayout(fig, [1 1]);
ax = uiaxes(g);
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(ax.Parent, g);
assert_isequal(g.Children, ax);
assert_istrue(isgraphics(ax.Layout));
assert_isequal(ax.Layout.Row, 1);
assert_isequal(ax.Layout.Column, 1);
delete(fig);
%=============================================================================
fig = uifigure('Visible', 'off');
tg = uitabgroup(fig);
assert_checkerror('uiaxes(tg)', getString(message('Nelson:axes:parentMustNotBeTabGroup')));
delete(fig);
%=============================================================================
% no parent: a new uifigure is created
close all
ax = uiaxes();
assert_isequal(class(ax), 'nelson.ui.control.UIAxes');
assert_isequal(class(ax.Parent), 'nelson.ui.Figure');
close all
%=============================================================================
% uiaxes reports a dedicated class but is still an axes and plots like one
fig = uifigure('Visible', 'off');
ax = uiaxes(fig);
assert_isequal(class(ax), 'nelson.ui.control.UIAxes');
assert_istrue(isa(ax, 'nelson.ui.control.UIAxes'));
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(ax.Type, 'axes');
delete(fig);
%=============================================================================
