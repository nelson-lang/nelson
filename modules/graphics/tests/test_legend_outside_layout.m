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
% Outside legend: the axes gives up legend size + tick inset + 8pt from its outer box,
% Position follows the loose inset and the legend sits the gap away at a constant pixel size.
gap = 8 * 96 / 72;
W = 480;
H = 420;
f = figure('Visible', 'off', 'Position', [100 100 W H]);
plot(1:10, 'DisplayName', 'G1');
hold on;
plot(10:-1:1, 'DisplayName', 'G2');
ax = gca;
drawnow();
basePosition = ax.Position;
baseOuter = ax.OuterPosition;
asserts.isapprox(basePosition, [0.13 0.11 0.775 0.815], 1e-12);
asserts.isapprox(baseOuter, [0 0 1 1], 1e-12);
lgd = legend('Location', 'northeastoutside');
drawnow();
legendPixelSize = lgd.Position(3:4) .* [W H];
locations = {'northeastoutside', 'southeastoutside', 'eastoutside', 'bestoutside', ...
  'northwestoutside', 'southwestoutside', 'westoutside', 'northoutside', 'southoutside'};
for k = 1:numel(locations)
  lgd.Location = locations{k};
  drawnow();
  assertOutsideLegendLayout(ax, lgd, locations{k}, W, H, gap);
  asserts.isapprox(lgd.Position(3:4) .* [W H], legendPixelSize, 1e-6);
  asserts.isequal(char(ax.PositionConstraint), 'outerposition');
end
% back inside gives the room back
lgd.Location = 'northeast';
drawnow();
asserts.isapprox(ax.Position, basePosition, 1e-12);
asserts.isapprox(ax.OuterPosition, baseOuter, 1e-12);
% so does deleting the legend
lgd.Location = 'eastoutside';
drawnow();
asserts.istrue(ax.Position(3) < basePosition(3));
delete(lgd);
drawnow();
asserts.isapprox(ax.Position, basePosition, 1e-12);
asserts.isapprox(ax.OuterPosition, baseOuter, 1e-12);
close(f);
%=============================================================================
% a resize keeps the legend pixel size and re-derives the slot for the new size
f = figure('Visible', 'off', 'Position', [100 100 W H]);
plot(1:10, 'DisplayName', 'G1');
hold on;
plot(10:-1:1, 'DisplayName', 'G2');
ax = gca;
lgd = legend('Location', 'northeastoutside');
drawnow();
legendPixelSize = lgd.Position(3:4) .* [W H];
sizes = [860 420; 480 600; 300 300; 624 546];
for k = 1:size(sizes, 1)
  f.Position = [100 100 sizes(k, :)];
  drawnow();
  assertOutsideLegendLayout(ax, lgd, 'northeastoutside', sizes(k, 1), sizes(k, 2), gap);
  asserts.isapprox(lgd.Position(3:4) .* sizes(k, :), legendPixelSize, 1e-6);
end
% a legend title grows the legend and the slot with it
title(lgd, 'Group');
drawnow();
asserts.istrue(lgd.Position(4) * sizes(end, 2) > legendPixelSize(2));
assertOutsideLegendLayout(ax, lgd, 'northeastoutside', sizes(end, 1), sizes(end, 2), gap);
close(f);
%=============================================================================
% a manual Position gives the slot from Position itself, keeping its left edge
f = figure('Visible', 'off', 'Position', [100 100 W H]);
plot(1:10, 'DisplayName', 'G1');
hold on;
plot(10:-1:1, 'DisplayName', 'G2');
ax = gca;
ax.Position = [0.2 0.2 0.6 0.6];
drawnow();
lgd = legend('Location', 'northeastoutside');
drawnow();
asserts.isequal(char(ax.PositionConstraint), 'innerposition');
tight = ax.TightInset;
slot = lgd.Position(3) * W + tight(3) * W + gap;
asserts.isapprox(ax.Position, [0.2 0.2 0.6 - slot / W, 0.6], 1e-9);
asserts.isapprox(lgd.Position(1) * W, (ax.Position(1) + ax.Position(3) + tight(3)) * W + gap, 1e-6);
asserts.isapprox(lgd.Position(2) + lgd.Position(4), ax.Position(2) + ax.Position(4), 1e-9);
lgd.Location = 'northeast';
drawnow();
asserts.isapprox(ax.Position, [0.2 0.2 0.6 0.6], 1e-12);
close(f);
%=============================================================================
% only the peer axes of the legend gives up room
f = figure('Visible', 'off', 'Position', [100 100 860 420]);
ax1 = subplot(1, 2, 1);
plot(ax1, 1:10, 'DisplayName', 'G1');
ax2 = subplot(1, 2, 2);
plot(ax2, 1:10);
drawnow();
position2 = ax2.Position;
width1 = ax1.Position(3);
lgd = legend(ax1, 'Location', 'northeastoutside');
drawnow();
asserts.istrue(ax1.Position(3) < width1);
asserts.isapprox(ax2.Position, position2, 1e-12);
asserts.istrue(lgd.Position(1) >= ax1.Position(1) + ax1.Position(3));
close(f);
%=============================================================================
% an outside legend and an outside colorbar share the side of the same axes
f = figure('Visible', 'off', 'Position', [100 100 860 420]);
plot(1:10, 'DisplayName', 'G1');
hold on;
plot(10:-1:1, 'DisplayName', 'G2');
ax = gca;
lgd = legend('Location', 'northeastoutside');
cb = colorbar('Location', 'eastoutside');
drawnow();
f.Position = [100 100 640 420];
drawnow();
axRight = ax.Position(1) + ax.Position(3);
asserts.istrue(lgd.Position(1) >= axRight);
asserts.istrue(cb.Position(1) >= axRight);
asserts.istrue(lgd.Position(1) + lgd.Position(3) <= 1);
asserts.istrue(cb.Position(1) + cb.Position(3) <= 1);
close(f);
%=============================================================================
function assertOutsideLegendLayout(ax, lgd, location, W, H, gap)
  tight = ax.TightInset;
  lgdPx = lgd.Position .* [W H W H];
  position = ax.Position .* [W H W H];
  isEast = contains(location, 'east') || strcmp(location, 'bestoutside');
  if isEast
    slot = lgdPx(3) + tight(3) * W + gap;
    expectedOuter = [0, 0, 1 - slot / W, 1];
    asserts.isapprox(lgdPx(1), position(1) + position(3) + tight(3) * W + gap, 1e-6);
  elseif contains(location, 'west')
    slot = lgdPx(3) + tight(1) * W + gap;
    expectedOuter = [slot / W, 0, 1 - slot / W, 1];
    asserts.isapprox(lgdPx(1) + lgdPx(3), position(1) - tight(1) * W - gap, 1e-6);
  elseif strcmp(location, 'northoutside')
    slot = lgdPx(4) + tight(4) * H + gap;
    expectedOuter = [0, 0, 1, 1 - slot / H];
    asserts.isapprox(lgdPx(2), position(2) + position(4) + tight(4) * H + gap, 1e-6);
  else
    slot = lgdPx(4) + tight(2) * H + gap;
    expectedOuter = [0, slot / H, 1, 1 - slot / H];
    asserts.isapprox(lgdPx(2) + lgdPx(4), position(2) - tight(2) * H - gap, 1e-6);
  end
  asserts.isapprox(ax.OuterPosition, expectedOuter, 1e-9);
  % Position is the loose inset of the outer box
  expectedPosition = [expectedOuter(1) + 0.13 * expectedOuter(3), ...
    expectedOuter(2) + 0.11 * expectedOuter(4), 0.775 * expectedOuter(3), 0.815 * expectedOuter(4)];
  asserts.isapprox(ax.Position, expectedPosition, 1e-9);
  if isEast || contains(location, 'west')
    if startsWith(location, 'north') || strcmp(location, 'bestoutside')
      asserts.isapprox(lgdPx(2) + lgdPx(4), position(2) + position(4), 1e-6);
    elseif startsWith(location, 'south')
      asserts.isapprox(lgdPx(2), position(2), 1e-6);
    else
      asserts.isapprox(lgdPx(2) + lgdPx(4) / 2, position(2) + position(4) / 2, 1e-6);
    end
  else
    asserts.isapprox(lgdPx(1) + lgdPx(3) / 2, position(1) + position(3) / 2, 1e-6);
  end
end
%=============================================================================
