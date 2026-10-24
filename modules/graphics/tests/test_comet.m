%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
x = 0:0.25:2;
y = sin(x);
expectedColors = ax.ColorOrder(1:3, :);
comet(ax, x, y, 0.25);
animated = findobj(ax, 'Type', 'animatedline');
marker = findobj(ax, 'Type', 'line');
asserts.isequal(numel(animated), 2);
asserts.isequal(numel(marker), 1);
asserts.isequal(marker.LineStyle, 'none');
asserts.isequal(marker.Marker, 'o');
asserts.isequal(marker.XData, x(end));
asserts.isequal(marker.YData, y(end));
asserts.isequal(marker.Tag, 'head');
tags = sort({animated.Tag});
asserts.isequal(tags(:), {'body'; 'tail'});
body = findobj(ax, 'Tag', 'body');
tail = findobj(ax, 'Tag', 'tail');
asserts.isequal(marker.Color, expectedColors(1, :));
asserts.isequal(body.Color, expectedColors(2, :));
asserts.isequal(tail.Color, expectedColors(3, :));
asserts.isequal(ax.ColorOrderIndex, 4);
asserts.isequal(char(f.DrawLater), 'off');
asserts.isequal(body.LineStyle, '-');
asserts.isequal(tail.LineStyle, '-');
asserts.isequal(graphics_comet_count_points(animated), sort([numel(x), max(1, round(0.25 * numel(x)))]));
asserts.istrue(graphics_comet_any_full_trace(animated, x, y));
close(f);
%=============================================================================
close all
%=============================================================================
