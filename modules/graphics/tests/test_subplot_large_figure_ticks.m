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
f = figure('Visible', 'off', 'Position', [100 100 1920 1020]);
X = linspace(-pi, pi) * 2;
Y1 = cos(X) .* exp(-2 * X);
Y2 = cos(X * 2) .* exp(-2 * X);
Y3 = cos(X * 3) .* exp(-2 * X);
Y4 = cos(X * 4) .* exp(-2 * X);
ax1 = subplot(4, 1, 1);
plot(X, Y1, 'b');
ax2 = subplot(4, 1, 2);
plot(X, Y2, 'r');
ax3 = subplot(4, 1, 3);
plot(X, Y3, 'g');
ax4 = subplot(4, 1, 4);
plot(X, Y4, 'k');
drawnow();
for ax = [ax1 ax2]
  asserts.isequal(ax.XLim, [-8 8]);
  asserts.isequal(ax.XTick, -8:2:8);
  asserts.isequal(ax.FontSize, 9.5);
  asserts.isequal(ax.YTick, 0:50000:300000);
  asserts.isequal(ax.YTickLabel, {'0'; '0.5'; '1'; '1.5'; '2'; '2.5'; '3'});
end
for ax = [ax3 ax4]
  asserts.isequal(ax.XLim, [-8 8]);
  asserts.isequal(ax.XTick, -8:2:8);
  asserts.isequal(ax.FontSize, 9.5);
  asserts.isequal(ax.YTick, [-100000 0 100000 200000 300000]);
  asserts.isequal(ax.YTickLabel, {'-1'; '0'; '1'; '2'; '3'});
end
close(f);
%=============================================================================
