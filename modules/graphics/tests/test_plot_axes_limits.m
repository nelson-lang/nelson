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
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
ax = axes('Parent', f);
plot(ax, 1:10, sin(1:10));
asserts.isequal(ax.XLim, [1 10]);
asserts.isequal(ax.XTick, 1:10);
asserts.isequal(ax.YLim, [-1 1]);
asserts.isapprox(ax.YTick, -1:0.2:1, 1e-15);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
ax = gca();
plot(ax, 1:10, tan(1:10));
plot(ax, 1:10, sin(1:10));
asserts.isequal(numel(ax.Children), 1);
asserts.isapprox(ax.Children(1).YData, sin(1:10), 1e-12);
asserts.isequal(ax.XLim, [1 10]);
asserts.isequal(ax.YLim, [-1 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
ax = axes('Parent', f);
plot(ax, 1e5:1e5:5e5, 1:5);
asserts.isequal(ax.XTickLabel, {'1'; '1.5'; '2'; '2.5'; '3'; '3.5'; '4'; '4.5'; '5'});
close(f);
close all;
%=============================================================================
