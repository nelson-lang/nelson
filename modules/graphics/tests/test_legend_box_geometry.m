%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% The legend box comes from its content only (rows, icon column in points, title band);
% Location never changes it.
W = 560;
H = 420;
f = figure('Visible', 'off', 'Position', [100 100 W H]);
hold on;
plot(1:3, 'DisplayName', 'G1');
plot(2:4, 'DisplayName', 'G2');
lgd = legend();
drawnow();
size0 = lgd.Position(3:4) .* [W H];
locations = {'north', 'south', 'east', 'west', 'northeast', 'northwest', 'southeast', ...
  'southwest', 'northoutside', 'southoutside', 'eastoutside', 'westoutside', ...
  'northeastoutside', 'northwestoutside', 'southeastoutside', 'southwestoutside', ...
  'best', 'bestoutside', 'northeast'};
for k = 1:numel(locations)
  lgd.Location = locations{k};
  drawnow();
  asserts.isapprox(lgd.Position(3:4) .* [W H], size0, 1e-9);
end
% Box off keeps the size.
lgd.Box = 'off';
drawnow();
asserts.isapprox(lgd.Position(3:4) .* [W H], size0, 1e-9);
lgd.Box = 'on';
% The icon column is given in points: 20 points less is 20 * 96 / 72 pixels less.
lgd.ItemTokenSize = [10 18];
drawnow();
asserts.isapprox(lgd.Position(3) * W, size0(1) - 20 * 96 / 72, 1e-9);
asserts.isapprox(lgd.Position(4) * H, size0(2), 1e-9);
lgd.ItemTokenSize = [30 18];
drawnow();
asserts.isapprox(lgd.Position(3:4) .* [W H], size0, 1e-9);
% A title adds one text line and 4 pixels; a short title does not widen the box.
title(lgd, 'Group');
drawnow();
titleSize = lgd.Position(3:4) .* [W H];
asserts.isapprox(titleSize(1), size0(1), 1e-9);
asserts.istrue(titleSize(2) > size0(2) + 10);
title(lgd, 'A longer legend title');
drawnow();
asserts.istrue(lgd.Position(3) * W > size0(1));
asserts.isapprox(lgd.Position(4) * H, titleSize(2), 1e-9);
close(f);
%=============================================================================
% One row per entry: the height grows by the same pitch for every entry.
heights = zeros(1, 3);
widths = zeros(1, 3);
for n = 1:3
  f = figure('Visible', 'off', 'Position', [100 100 W H]);
  hold on;
  for k = 1:n
    plot(1:3, k * (1:3), 'DisplayName', sprintf('G%d', k));
  end
  lgd = legend();
  drawnow();
  heights(n) = lgd.Position(4) * H;
  widths(n) = lgd.Position(3) * W;
  close(f);
end
pitch = heights(2) - heights(1);
asserts.isapprox(heights(3) - heights(2), pitch, 1e-9);
asserts.istrue(pitch > 12 && pitch < 16);
asserts.isapprox(widths, widths(1) * [1 1 1], 1e-9);
% The title band is one row pitch plus 3.5 pixels.
f = figure('Visible', 'off', 'Position', [100 100 W H]);
hold on;
plot(1:3, 'DisplayName', 'G1');
plot(2:4, 'DisplayName', 'G2');
lgd = legend();
drawnow();
h0 = lgd.Position(4) * H;
title(lgd, 'Group');
drawnow();
asserts.isapprox(lgd.Position(4) * H - h0, pitch + 3.5, 1e-9);
close(f);
%=============================================================================
% Columns and a horizontal orientation repeat the full column width.
f = figure('Visible', 'off', 'Position', [100 100 W H]);
hold on;
for k = 1:4
  plot(1:3, k * (1:3), 'DisplayName', sprintf('G%d', k));
end
lgd = legend('NumColumns', 2);
drawnow();
asserts.isapprox(lgd.Position(3) * W, 2 * widths(1), 1e-9);
asserts.isapprox(lgd.Position(4) * H, heights(2), 1e-9);
close(f);
f = figure('Visible', 'off', 'Position', [100 100 W H]);
hold on;
for k = 1:4
  plot(1:3, k * (1:3), 'DisplayName', sprintf('G%d', k));
end
lgd = legend('Orientation', 'horizontal');
drawnow();
asserts.isapprox(lgd.Position(3) * W, 4 * widths(1), 1e-9);
asserts.isapprox(lgd.Position(4) * H, heights(1), 1e-9);
close(f);
%=============================================================================
% Marker-only entries only need the marker: the icon column shrinks.
f = figure('Visible', 'off', 'Position', [100 100 W H]);
hold on;
plot(nan, nan, 'o', 'MarkerFaceColor', 'b', 'DisplayName', 'G1');
plot(nan, nan, 'o', 'MarkerFaceColor', 'r', 'DisplayName', 'G2');
lgd = legend();
drawnow();
asserts.istrue(lgd.Position(3) * W < widths(2) - 20);
asserts.isapprox(lgd.Position(4) * H, heights(2), 1e-9);
% one line among the markers brings the full icon column back
plot(1:3, 'DisplayName', 'L');
drawnow();
asserts.istrue(lgd.Position(3) * W >= widths(2));
close(f);
%=============================================================================
