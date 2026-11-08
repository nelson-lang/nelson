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
% Like the reference, the default ItemHitFcn starts text editing when a legend label is double-clicked:
% an edit box opens over the label, and its callback renames the entry and the plotted object.
%=============================================================================
f = figure('Position', [100 100 400 300]);
ax = axes('Parent', f, 'Position', [0.13 0.11 0.775 0.815]);
h1 = plot(ax, 1:10, 'DisplayName', 'linear');
hold(ax, 'on');
h2 = plot(ax, (1:10) .^ 2, 'DisplayName', 'square');
lg = legend(ax);
drawnow();
W = f.Position(3);
H = f.Position(4);
p = lg.Position;
% row pitch is one text line (1.117 em) plus half a pixel, rows start 1.75 px
% under the top edge; the icon column is 30 points, the label follows a 16/3 px gap
rowH = 1.117 * lg.FontSize * 96 / 72 + 0.5;
y0 = p(2) * H + p(4) * H - 1.75;
baseX = p(1) * W + 4;
iconW = 30 * 96 / 72;
labelPoint = [baseX + iconW + 16 / 3 + 5, y0 - rowH * 1.5];
iconPoint = [baseX + 10, y0 - rowH * 1.5];
%=============================================================================
% a normal click on a label, or a double-click on an icon, does not edit
__figure_mouse_event__(f, 'press', labelPoint, 'web');
drawnow();
pause(0.2);
assert_isequal(numel(findall(f, 'Tag', 'LegendLabelEditor')), 0);
__figure_mouse_event__(f, 'press', iconPoint, 'web', 'open');
drawnow();
pause(0.2);
assert_isequal(numel(findall(f, 'Tag', 'LegendLabelEditor')), 0);
%=============================================================================
% a double-click on a label opens an editor holding the current text
__figure_mouse_event__(f, 'press', labelPoint, 'web', 'open');
drawnow();
pause(0.3);
editor = findall(f, 'Tag', 'LegendLabelEditor');
assert_isequal(numel(editor), 1);
assert_isequal(editor.String, 'square');
%=============================================================================
% committing the edit renames the legend entry and the plotted object
editor.String = 'renamed';
callback = editor.Callback;
callback(editor, []);
drawnow();
assert_isequal(lg.String, {'linear', 'renamed'});
assert_isequal(h2.DisplayName, 'renamed');
assert_isequal(h1.DisplayName, 'linear');
assert_isequal(numel(findall(f, 'Tag', 'LegendLabelEditor')), 0);
%=============================================================================
% a custom ItemHitFcn replaces the default behavior
lg.ItemHitFcn = @(src, evt) disp('');
__figure_mouse_event__(f, 'press', labelPoint, 'web', 'open');
drawnow();
pause(0.2);
assert_isequal(numel(findall(f, 'Tag', 'LegendLabelEditor')), 0);
close(f);
%=============================================================================
