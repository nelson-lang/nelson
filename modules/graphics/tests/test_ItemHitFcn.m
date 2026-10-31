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
% ItemHitFcn fires when the user clicks a legend entry, with that entry's
% plotted object as evt.Peer and the clicked Region ('icon' or 'label'). The
% click is replayed through the figure input with __figure_mouse_event__.
%=============================================================================
% the callback is a settable function handle / char that round-trips
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10);
hold(ax, 'on');
plot(ax, (1:10) .^ 2);
lg = legend(ax, {'linear', 'square'});
assert_isequal(class(lg.ItemHitFcn), 'function_handle');
lg.ItemHitFcn = @(src, evt) disp('hit');
assert_isequal(class(lg.ItemHitFcn), 'function_handle');
lg.ItemHitFcn = 'disp(''hit'')';
assert_isequal(class(lg.ItemHitFcn), 'char');
close(f);
%=============================================================================
% end-to-end firing: a click on an entry resolves to its Peer and Region
global ITEM_HIT_EVENTS
ITEM_HIT_EVENTS = {};
f = figure('Position', [100 100 400 300]);
ax = axes('Parent', f, 'Position', [0.13 0.11 0.775 0.815]);
h1 = plot(ax, 1:10, 'DisplayName', 'linear');
hold(ax, 'on');
h2 = plot(ax, (1:10) .^ 2, 'DisplayName', 'square');
lg = legend(ax);
lg.ItemHitFcn = @(src, evt) record_item_hit(evt);
drawnow();
%=============================================================================
W = f.Position(3);
H = f.Position(4);
p = lg.Position;                        % normalized [x y w h], bottom-origin
rowH = max(10, ceil(lg.FontSize * 1.35));
y0 = p(2) * H + p(4) * H - 4;
baseX = p(1) * W + 6;
%=============================================================================
% entry 0 (top row), icon column -> Peer is the first plot, Region 'icon'
ITEM_HIT_EVENTS = {};
__figure_mouse_event__(f, 'press', [baseX + 3, y0 - rowH * 0.5], 'web');
drawnow();
pause(0.2);
assert_isequal(numel(ITEM_HIT_EVENTS), 1);
assert_isequal(ITEM_HIT_EVENTS{1}.Peer, h1);
assert_isequal(ITEM_HIT_EVENTS{1}.Region, 'icon');
%=============================================================================
% entry 1 (second row), label column -> Peer is the second plot, Region 'label'
ITEM_HIT_EVENTS = {};
__figure_mouse_event__(f, 'press', [baseX + 40, y0 - rowH * 1.5], 'web');
drawnow();
pause(0.2);
assert_isequal(numel(ITEM_HIT_EVENTS), 1);
assert_isequal(ITEM_HIT_EVENTS{1}.Peer, h2);
assert_isequal(ITEM_HIT_EVENTS{1}.Region, 'label');
%=============================================================================
close(f);
clear global ITEM_HIT_EVENTS
%=============================================================================
function record_item_hit(evt)
  global ITEM_HIT_EVENTS
  ITEM_HIT_EVENTS{end + 1} = evt;
end
%=============================================================================
