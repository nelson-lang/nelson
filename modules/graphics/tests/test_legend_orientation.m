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
lgd = legend(ax, 'A');
legend(ax, 'hide');
asserts.isequal(char(lgd.Visible), 'off');
legend(ax, 'toggle');
asserts.isequal(char(lgd.Visible), 'on');
legend(ax, 'toggle');
asserts.isequal(char(lgd.Visible), 'off');
legend(ax, 'show');
asserts.isequal(char(lgd.Visible), 'on');
legend(ax, 'boxoff');
asserts.isequal(char(lgd.Box), 'off');
legend(ax, 'boxon');
asserts.isequal(char(lgd.Box), 'on');
legend(ax, 'delete');
asserts.isfalse(isgraphics(lgd));
asserts.istrue(isempty(ax.Legend));
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
