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
close all
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
x = [1 2 3 4];
y = [2 4 6 8];
z = [-1 0 1 2];
expectedColors = ax.ColorOrder(1:3, :);
comet3(ax, x, y, z, 0.25);
animated = findobj(ax, 'Type', 'animatedline');
head = findobj(ax, 'Tag', 'head');
body = findobj(ax, 'Tag', 'body');
tail = findobj(ax, 'Tag', 'tail');
asserts.isequal(numel(animated), 2);
asserts.isequal(numel(head), 1);
asserts.isequal(head.Type, 'line');
asserts.isequal(head.Marker, 'o');
asserts.isequal(head.XData, x(end));
asserts.isequal(head.YData, y(end));
asserts.isequal(head.ZData, z(end));
asserts.isequal(body.Type, 'animatedline');
asserts.isequal(tail.Type, 'animatedline');
asserts.isequal(head.Color, expectedColors(1, :));
asserts.isequal(body.Color, expectedColors(2, :));
asserts.isequal(tail.Color, expectedColors(3, :));
asserts.isequal(ax.ColorOrderIndex, 4);
asserts.isequal(char(f.DrawLater), 'off');
asserts.isequal(body.LineStyle, '-');
asserts.isequal(tail.LineStyle, '-');
asserts.isequal(graphics_comet3_count_points(animated), [1 4]);
asserts.istrue(graphics_comet3_any_full_trace(animated, x, y, z));
asserts.isequal(ax.XLim, [1 4]);
asserts.isequal(ax.YLim, [2 8]);
asserts.isequal(ax.ZLim, [-1 2]);
close(f);
%=============================================================================
close all
%=============================================================================
