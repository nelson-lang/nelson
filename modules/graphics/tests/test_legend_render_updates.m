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
close all
%=============================================================================
[f, ax] = make_render_axes();
x = linspace(0, 10, 40);
plot(ax, x, sin(x), 'r-o', 'LineWidth', 1.5, 'MarkerFaceColor', 'y');
hold(ax, 'on');
plot(ax, x, cos(x), 'b--s', 'LineWidth', 1.5, 'MarkerFaceColor', 'c');
grid(ax, 'on');
lgd = legend(ax, {'sin(x)'; 'cos(x)'}, 'Location', 'northeast');

lgd.AutoUpdate = 'off';
plot(ax, x, 0.5 * sin(x), 'k:', 'DisplayName', 'extra');
drawnow();
asserts.isequal(lgd.String, {'sin(x)'; 'cos(x)'});
lgd.BackgroundAlpha = 0.35;
asserts.isequal(lgd.BackgroundAlpha, 0.35);
lgd.Color = [1 1 0.75];
asserts.isequal(lgd.Color, [1 1 0.75]);
lgd.Box = 'off';
asserts.isequal(char(lgd.Box), 'off');
lgd.BusyAction = 'cancel';
asserts.isequal(lgd.BusyAction, 'cancel');
lgd.ButtonDownFcn = 'disp(1)';
asserts.isequal(lgd.ButtonDownFcn, 'disp(1)');
asserts.isequal(length(lgd.Children), 0);
asserts.isequal(lgd.ContextMenu, gobjects(0, 0));
lgd.CreateFcn = [];
asserts.isequal(lgd.CreateFcn, '');
lgd.DeleteFcn = [];
asserts.isequal(lgd.DeleteFcn, '');
lgd.Direction = 'reverse';
asserts.isequal(lgd.Direction, 'reverse');
asserts.isequal(lgd.DirectionMode, 'manual');
lgd.EdgeColor = [0 0.45 0.74];
asserts.isequal(lgd.EdgeColor, [0 0.45 0.74]);
lgd.FontAngle = 'italic';
asserts.isequal(lgd.FontAngle, 'italic');
lgd.FontName = 'Arial';
asserts.isequal(lgd.FontName, 'Arial');
lgd.FontSize = 12;
asserts.isequal(lgd.FontSize, 12);
lgd.FontWeight = 'bold';
asserts.isequal(lgd.FontWeight, 'bold');
lgd.HandleVisibility = 'off';
asserts.isequal(char(lgd.HandleVisibility), 'off');
lgd.HitTest = 'off';
asserts.isequal(char(lgd.HitTest), 'off');
lgd.IconColumnWidth = 45;
asserts.isequal(lgd.IconColumnWidth, 45);
asserts.isequal(lgd.IconColumnWidthMode, 'manual');
lgd.String = {'x_1'; 'x_2'};
lgd.Interpreter = 'none';
asserts.isequal(lgd.String, {'x_1'; 'x_2'});
asserts.isequal(lgd.Interpreter, 'none');
lgd.Interruptible = 'off';
asserts.isequal(char(lgd.Interruptible), 'off');
lgd.ItemHitFcn = [];
asserts.isequal(lgd.ItemHitFcn, '');
asserts.istrue(isgraphics(lgd.Layout, 'layoutoptions'));
asserts.isequal(lgd.Layout.Parent, lgd);
lgd.LineWidth = 2;
asserts.isequal(lgd.LineWidth, 2);
lgd.Location = 'southwest';
asserts.isequal(lgd.Location, 'southwest');
lgd.NumColumns = 2;
asserts.isequal(lgd.NumColumns, 2);
asserts.isequal(lgd.NumColumnsMode, 'manual');
lgd.Orientation = 'horizontal';
asserts.isequal(lgd.Orientation, 'horizontal');
asserts.isequal(lgd.Parent, f);
lgd.PickableParts = 'none';
asserts.isequal(lgd.PickableParts, 'none');
lgd.Position = [0.2 0.2 0.25 0.12];
asserts.isequal(lgd.Location, 'none');
asserts.isapprox(lgd.Position, [0.2 0.2 0.25 0.12], 1e-12);
lgd.Selected = 'on';
asserts.isequal(char(lgd.Selected), 'on');
lgd.SelectionHighlight = 'off';
asserts.isequal(char(lgd.SelectionHighlight), 'off');
lgd.String = {'alpha'; 'beta'};
asserts.isequal(lgd.String, {'alpha'; 'beta'});
lgd.Tag = 'legend-render';
asserts.isequal(lgd.Tag, 'legend-render');
lgd.TextColor = [0.55 0 0];
asserts.isequal(lgd.TextColor, [0.55 0 0]);
title(lgd, 'Legend title');
asserts.isequal(lgd.Title.String, 'Legend title');
asserts.isequal(lgd.Type, 'legend');
lgd.Units = 'pixels';
lgd.Position = [370 320 120 45];
asserts.isequal(lgd.Units, 'pixels');
asserts.isapprox(lgd.Position, [370 320 120 45], 1e-12);
lgd.UserData = magic(2);
asserts.isequal(lgd.UserData, magic(2));
drawnow();
frame = getframe(f);
asserts.isequal(size(frame.cdata, 3), 3);
lgd.Visible = 'off';
asserts.isequal(char(lgd.Visible), 'off');
close(f);
%=============================================================================
function [f, ax] = make_axes()
  f = figure('Visible', 'off', 'DrawLater', 'on');
  ax = axes('Parent', f);
end
%=============================================================================
function [f, ax, h1, h2] = make_two_line_axes()
  [f, ax] = make_axes();
  x = linspace(0, pi);
  h1 = plot(ax, x, cos(x));
  hold(ax, 'on');
  h2 = plot(ax, x, cos(2 * x));
end
%=============================================================================
function [f, ax] = make_render_axes()
  f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420], 'Color', [1 1 1]);
  ax = axes('Parent', f);
end
%=============================================================================
function assert_required_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
end
%=============================================================================
function names = reference_legend_properties()
  names = {'Title'; 'AutoUpdate'; 'Box'; 'BackgroundAlpha'; 'Color'; 'EdgeColor'; ...
    'FontName'; 'FontSize'; 'FontAngle'; 'FontWeight'; 'Interpreter'; 'ItemHitFcn'; ...
    'LineWidth'; 'Location'; 'NumColumns'; 'NumColumnsMode'; 'Orientation'; ...
    'IconColumnWidth'; 'IconColumnWidthMode'; 'Direction'; 'DirectionMode'; ...
    'Position'; 'String'; 'TextColor'; 'Units'; 'Children'; 'Parent'; 'Visible'; ...
    'HandleVisibility'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted'; ...
    'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected'; ...
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'Layout'};
end
%=============================================================================
function assert_legend_swatch_color(cdata, position, rowIndex, expectedColor)
  figureHeight = size(cdata, 1);
  figureWidth = size(cdata, 2);
  rowHeight = ceil(9 * 1.35);
  xmin = max(1, round(position(1) * figureWidth) + 4);
  xmax = min(figureWidth, round(position(1) * figureWidth) + 35);
  legendTop = round(figureHeight - (position(2) + position(4)) * figureHeight);
  ymin = max(1, legendTop + (rowIndex - 1) * rowHeight);
  ymax = min(figureHeight, ymin + rowHeight - 1);
  region = double(cdata(ymin:ymax, xmin:xmax, 1:3));
  redScore = max(max(region(:, :, 1) - region(:, :, 3) - region(:, :, 2) / 2));
  blueScore = max(max(region(:, :, 3) - region(:, :, 1) - region(:, :, 2) / 2));
  [~, dominant] = max([redScore, 0, blueScore]);
  [~, expected] = max(expectedColor);
  asserts.isequal(dominant, expected);
end
%=============================================================================
