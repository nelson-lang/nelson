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
[f, ax] = make_axes();
plot(ax, 1:3);
hold(ax, 'on');
plot(ax, 2:4);
lgd = legend(ax, {'A'; 'B'});
lgd.AutoUpdate = 'off';
asserts.isequal(char(lgd.AutoUpdate), 'off');
lgd.BackgroundAlpha = 0.5;
asserts.isequal(lgd.BackgroundAlpha, 0.5);
lgd.Box = 'off';
asserts.isequal(char(lgd.Box), 'off');
lgd.Color = [0.1 0.2 0.3];
asserts.isequal(lgd.Color, [0.1 0.2 0.3]);
lgd.EdgeColor = 'red';
asserts.isequal(lgd.EdgeColor, [1 0 0]);
lgd.TextColor = [0.4 0.5 0.6];
asserts.isequal(lgd.TextColor, [0.4 0.5 0.6]);
lgd.FontName = 'Arial';
asserts.isequal(lgd.FontName, 'Arial');
lgd.FontSize = 12;
asserts.isequal(lgd.FontSize, 12);
lgd.FontAngle = 'italic';
asserts.isequal(lgd.FontAngle, 'italic');
lgd.FontWeight = 'bold';
asserts.isequal(lgd.FontWeight, 'bold');
lgd.Interpreter = 'none';
asserts.isequal(lgd.Interpreter, 'none');
lgd.LineWidth = 2;
asserts.isequal(lgd.LineWidth, 2);
lgd.Location = 'southwest';
asserts.isequal(lgd.Location, 'southwest');
lgd.NumColumns = 2;
asserts.isequal(lgd.NumColumns, 2);
asserts.isequal(lgd.NumColumnsMode, 'manual');
lgd.Orientation = 'horizontal';
asserts.isequal(lgd.Orientation, 'horizontal');
lgd.IconColumnWidth = 40;
asserts.isequal(lgd.IconColumnWidth, 40);
asserts.isequal(lgd.IconColumnWidthMode, 'manual');
lgd.Direction = 'reverse';
asserts.isequal(lgd.Direction, 'reverse');
asserts.isequal(lgd.DirectionMode, 'manual');
lgd.Position = [0.2 0.2 0.3 0.2];
asserts.isequal(lgd.Location, 'none');
asserts.isapprox(lgd.Position, [0.2 0.2 0.3 0.2], 1e-12);
lgd.Units = 'pixels';
asserts.isequal(lgd.Units, 'pixels');
lgd.Units = 'normalized';
asserts.isequal(lgd.Units, 'normalized');
lgd.String = {'C'; 'D'};
asserts.isequal(lgd.String, {'C', 'D'});
lgd.Visible = 'off';
asserts.isequal(char(lgd.Visible), 'off');
lgd.Visible = 'on';
lgd.HandleVisibility = 'off';
asserts.isequal(char(lgd.HandleVisibility), 'off');
lgd.UserData = magic(2);
asserts.isequal(lgd.UserData, magic(2));
lgd.Selected = 'on';
asserts.isequal(char(lgd.Selected), 'on');
lgd.SelectionHighlight = 'off';
asserts.isequal(char(lgd.SelectionHighlight), 'off');
lgd.HitTest = 'off';
asserts.isequal(char(lgd.HitTest), 'off');
lgd.PickableParts = 'none';
asserts.isequal(lgd.PickableParts, 'none');
lgd.BusyAction = 'cancel';
asserts.isequal(lgd.BusyAction, 'cancel');
lgd.Interruptible = 'on';
asserts.isequal(char(lgd.Interruptible), 'on');
lgd.ContextMenu = gobjects(0, 0);
asserts.isequal(lgd.ContextMenu, gobjects(0, 0));
lgd.ButtonDownFcn = 'disp(1)';
asserts.isequal(lgd.ButtonDownFcn, 'disp(1)');
lgd.ItemHitFcn = 'disp(2)';
asserts.isequal(lgd.ItemHitFcn, 'disp(2)');
lgd.CreateFcn = 'disp(3)';
asserts.isequal(lgd.CreateFcn, 'disp(3)');
lgd.DeleteFcn = 'disp(4)';
asserts.isequal(lgd.DeleteFcn, 'disp(4)');
lgd.DeleteFcn = [];
title(lgd, 'Legend title');
asserts.isequal(lgd.Title.String, 'Legend title');
asserts.checkerror('lgd.Type = ''axes'';', graphicsPropertyErrorMessage('Type', 'Nelson:graphics:readOnlyProperty', 'Type'));
asserts.checkerror('lgd.Title = [];', graphicsPropertyErrorMessage('Title', 'Nelson:graphics:readOnlyProperty', 'Title'));
asserts.checkerror('lgd.BeingDeleted = ''on'';', graphicsPropertyErrorMessage('BeingDeleted', 'Nelson:graphics:readOnlyProperty', 'BeingDeleted'));
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
