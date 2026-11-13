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
% A legend of an axes held by a uitab or a uipanel belongs to that container
% and sits next to the axes, in the container units.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 700 500]);
p = uipanel(f, 'Units', 'pixels', 'Position', [50 50 400 300]);
ax = axes('Parent', p);
h = plot(ax, 1:10);
lg = legend(ax, 'data1');
drawnow();
assert_istrue(isequal(lg.Parent, p));
assert_istrue(any(p.Children == lg));
assert_isfalse(any(f.Children == lg));
assert_isequal(lg.Location, 'northeast');
axPos = ax.Position;
lgPos = lg.Position;
% Inside the top right corner of the axes box.
axRight = axPos(1) + axPos(3);
axTop = axPos(2) + axPos(4);
assert_istrue(lgPos(1) + lgPos(3) < axRight);
assert_istrue(lgPos(1) + lgPos(3) > axRight - 0.06);
assert_istrue(lgPos(2) + lgPos(4) < axTop);
assert_istrue(lgPos(2) + lgPos(4) > axTop - 0.08);
% Units are relative to the panel content, not to the figure.
ax.Units = 'pixels';
contentWidth = ax.Position(3) / axPos(3);
ax.Units = 'normalized';
lg.Units = 'pixels';
lgPixels = lg.Position;
lg.Units = 'normalized';
assert_isapprox(lgPixels(3), lgPos(3) * contentWidth, 1);
assert_istrue(contentWidth < 401);
%=============================================================================
% A click on the entry fires ItemHitFcn through the panel.
global ITEM_HIT_EVENTS
ITEM_HIT_EVENTS = {};
lg.ItemHitFcn = @(src, evt) record_item_hit(evt);
clickX = 50 + lgPixels(1) + lgPixels(3) * 0.75;
clickY = 50 + lgPixels(2) + lgPixels(4) * 0.5;
__figure_mouse_event__(f, 'press', [clickX, clickY], 'web');
drawnow();
pause(0.2);
assert_isequal(numel(ITEM_HIT_EVENTS), 1);
assert_isequal(ITEM_HIT_EVENTS{1}.Peer, h);
clear global ITEM_HIT_EVENTS
%=============================================================================
% 'northeastoutside' shrinks the axes and puts the legend at its right.
lg.Location = 'northeastoutside';
drawnow();
axPos = ax.Position;
lgPos = lg.Position;
assert_istrue(axPos(3) < 0.7);
assert_istrue(lgPos(1) > axPos(1) + axPos(3));
assert_istrue(lgPos(1) + lgPos(3) < 1);
assert_isapprox(lgPos(2) + lgPos(4), axPos(2) + axPos(4), 1e-2);
% The frame shows the legend: a red legend background lands where the panel is gray.
lg.Color = [1 0 0];
img = getframe(f);
[hImg, wImg, ~] = size(img.cdata);
lg.Units = 'pixels';
lgPixels = lg.Position;
lg.Units = 'normalized';
x = round((50 + lgPixels(1) + 3) * wImg / 700);
y = round(hImg - (50 + lgPixels(2) + lgPixels(4) / 4) * hImg / 500);
rgb = double(squeeze(img.cdata(y, x, :)));
assert_istrue(rgb(1) > 200 && rgb(2) < 80 && rgb(3) < 80);
%=============================================================================
% legend(ax) again reuses it; 'off' removes it from the panel.
lg2 = legend(ax);
assert_istrue(isequal(lg2, lg));
assert_isequal(numel(findall(p, 'Type', 'legend')), 1);
legend(ax, 'off');
assert_istrue(isempty(findall(p, 'Type', 'legend')));
lg = legend(ax);
assert_istrue(isequal(lg.Parent, p));
% Deleting the axes deletes its legend.
delete(ax);
assert_isfalse(isgraphics(lg));
assert_istrue(isempty(p.Children));
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 700 500]);
tg = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
tb = uitab(tg, 'Title', 'T1');
ax = axes('Parent', tb);
plot(ax, 1:10);
lg = legend(ax, 'a');
assert_istrue(isequal(lg.Parent, tb));
lgPos = lg.Position;
axPos = ax.Position;
assert_istrue(lgPos(1) + lgPos(3) < axPos(1) + axPos(3));
assert_istrue(lgPos(2) + lgPos(4) < axPos(2) + axPos(4));
% A colorbar and a legend of the same axes live side by side in the tab.
cb = colorbar(ax);
assert_istrue(isequal(cb.Parent, tb));
assert_isequal(numel(tb.Children), 3);
delete(ax);
assert_isfalse(isgraphics(lg));
assert_isfalse(isgraphics(cb));
close(f);
%=============================================================================
% bubblelegend follows the same rule.
f = figure('Visible', 'off', 'Position', [100 100 700 500]);
p = uipanel(f, 'Units', 'pixels', 'Position', [50 50 400 300]);
ax = axes('Parent', p);
bubblechart(ax, 1:3, 1:3, [1 5 10]);
bl = bubblelegend(ax);
assert_istrue(isequal(bl.Parent, p));
assert_isfalse(any(f.Children == bl));
delete(ax);
assert_isfalse(isgraphics(bl));
close(f);
%=============================================================================
function record_item_hit(evt)
  global ITEM_HIT_EVENTS
  ITEM_HIT_EVENTS{end + 1} = evt;
end
%=============================================================================
