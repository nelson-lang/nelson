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
[f, ax, h1, h2] = make_two_line_axes();
[lgd, icons, plotHandles, textLabels] = legend(ax, {'cos(x)'; 'cos(2x)'}, 'Location', 'NE');
asserts.istrue(isgraphics(lgd, 'legend'));
asserts.isequal(lgd.Type, 'legend');
asserts.isequal(lgd.Tag, 'legend');
asserts.isequal(lgd.Parent, f);
asserts.isequal(length(lgd.Children), 0);
asserts.istrue(isgraphics(lgd.Title, 'text'));
asserts.isequal(lgd.Title.Parent, lgd);
asserts.isequal(lgd.Title.String, '');
asserts.istrue(isgraphics(lgd.Layout, 'layoutoptions'));
asserts.isequal(lgd.String, {'cos(x)', 'cos(2x)'});
asserts.isequal(ax.Legend, lgd);
asserts.isequal(plotHandles, [h1; h2]);
asserts.isequal(size(icons), [0 1]);
asserts.isequal(textLabels, {'cos(x)', 'cos(2x)'});
assert_required_properties(lgd, reference_legend_properties());
s = get(lgd);
asserts.istrue(isstruct(s));
asserts.istrue(isfield(s, 'Title'));
asserts.isequal(char(lgd.AutoUpdate), 'on');
asserts.isequal(lgd.BackgroundAlpha, 1);
asserts.isequal(char(lgd.BeingDeleted), 'off');
asserts.isequal(char(lgd.Box), 'on');
asserts.isequal(lgd.BusyAction, 'queue');
asserts.isequal(lgd.ButtonDownFcn, '');
asserts.isequal(lgd.Color, [1 1 1]);
asserts.isequal(lgd.ContextMenu, gobjects(0, 0));
asserts.isequal(lgd.CreateFcn, '');
asserts.isequal(lgd.DeleteFcn, '');
asserts.isequal(lgd.Direction, 'normal');
asserts.isequal(lgd.DirectionMode, 'auto');
asserts.isapprox(lgd.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-12);
asserts.isequal(lgd.FontAngle, 'normal');
asserts.isequal(lgd.FontName, 'Helvetica');
asserts.isequal(lgd.FontSize, 9);
asserts.isequal(lgd.FontWeight, 'normal');
asserts.isequal(char(lgd.HandleVisibility), 'on');
asserts.isequal(char(lgd.HitTest), 'on');
asserts.isequal(lgd.IconColumnWidth, 30);
asserts.isequal(lgd.IconColumnWidthMode, 'auto');
asserts.isequal(lgd.Interpreter, 'tex');
asserts.isequal(char(lgd.Interruptible), 'off');
asserts.isequal(class(lgd.ItemHitFcn), 'function_handle');
asserts.isequal(lgd.LineWidth, 0.5);
asserts.isequal(lgd.Location, 'northeast');
asserts.isequal(lgd.NumColumns, 1);
asserts.isequal(lgd.NumColumnsMode, 'auto');
asserts.isequal(lgd.Orientation, 'vertical');
asserts.isequal(lgd.PickableParts, 'visible');
asserts.isequal(char(lgd.Selected), 'off');
asserts.isequal(char(lgd.SelectionHighlight), 'on');
asserts.isapprox(lgd.TextColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-12);
asserts.isequal(lgd.Units, 'normalized');
asserts.isequal(lgd.UserData, []);
asserts.isequal(char(lgd.Visible), 'on');
titlePositionBefore = lgd.Position;
title(lgd, 'Legend title');
titlePositionAfter = lgd.Position;
asserts.isequal(lgd.Title.String, 'Legend title');
asserts.istrue(titlePositionAfter(2) < titlePositionBefore(2));
asserts.istrue(titlePositionAfter(4) > titlePositionBefore(4));
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
