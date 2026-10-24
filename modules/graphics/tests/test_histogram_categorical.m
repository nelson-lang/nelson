%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
% One bar per category, counts in category order, category names as labels.
f = figure('Visible', 'off');
X = categorical({'a', 'b', 'a', 'c', 'b', 'a'});
h = histogram(X);
asserts.istrue(isgraphics(h, 'histogram'));
asserts.isequal(h.Type, 'histogram');
asserts.isequal(h.Categories, {'a', 'b', 'c'});
asserts.isequal(h.BinCounts, [3 2 1]);
asserts.isequal(h.Values, [3 2 1]);
asserts.isequal(h.BarWidth, 0.9);
ax = gca;
asserts.isequal(ax.XTick, [1 2 3]);
asserts.isequal(ax.XTickLabel, {'a'; 'b'; 'c'});
drawnow();
asserts.isequal(ax.XLim, [0.5 3.5]);
asserts.isequal(ax.YLim, [0 3]);
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
% Categories keep the sorted order regardless of the data order.
f = figure('Visible', 'off');
X = categorical({'small', 'medium', 'large', 'small'});
h = histogram(X);
asserts.isequal(h.Categories, {'large', 'medium', 'small'});
asserts.isequal(h.BinCounts, [1 1 2]);
ax = gca;
asserts.isequal(ax.XTickLabel, {'large'; 'medium'; 'small'});
close(f);
%=============================================================================
% Explicit category subset / order as a second argument.
f = figure('Visible', 'off');
X = categorical({'a', 'b', 'a', 'c', 'b', 'a'});
h = histogram(X, {'c', 'a'});
asserts.isequal(h.Categories, {'c', 'a'});
asserts.isequal(h.BinCounts, [1 3]);
ax = gca;
asserts.isequal(ax.XTickLabel, {'c'; 'a'});
close(f);
%=============================================================================
% Horizontal orientation places category labels on the Y axis.
f = figure('Visible', 'off');
X = categorical({'a', 'b', 'a'});
h = histogram(X, 'Orientation', 'horizontal');
asserts.isequal(h.Orientation, 'horizontal');
ax = gca;
asserts.isequal(ax.YTick, [1 2]);
asserts.isequal(ax.YTickLabel, {'a'; 'b'});
close(f);
%=============================================================================
close all
%=============================================================================
