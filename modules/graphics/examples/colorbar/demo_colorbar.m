%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
figure('Color', [1 1 1]);
t = tiledlayout(2, 2);

ax1 = nexttile(t);
surf(peaks);
view(35, 28);
set(ax1, 'XLim', [0 60], 'YLim', [0 50], ...
  'XTick', [0 20 40 60], 'YTick', [0 50], 'ZTick', [-10 -5 0 5 10]);
colormap(ax1, parula(64));
colorbar(ax1);
title(ax1, 'Surface');

ax2 = nexttile(t);
contourf(peaks);
set(ax2, 'YTick', 10:10:40);
colormap(ax2, parula(64));
colorbar(ax2, 'southoutside');
title(ax2, 'Horizontal');

ax3 = nexttile(t);
imagesc(peaks);
colormap(ax3, turbo(64));
cb = colorbar(ax3, 'Ticks', [-6 -3 0 3 6], ...
  'TickLabels', {'low'; '-3'; '0'; '3'; 'high'});
cb.Label.String = 'Scale';
cb.Direction = 'reverse';
title(ax3, 'Ticks');

ax4 = nexttile(t);
pcolor(peaks);
set(ax4, 'YTick', 10:10:40, 'Box', 'on');
colormap(ax4, parula(64));
cb = colorbar(ax4, 'Location', 'layout');
cb.Layout.Tile = 'east';
title(ax4, 'Layout');
