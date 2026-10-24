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
skip_if_portable_graphics_backend('native tab group layout metrics differ in portable rendering.');
% uitabgroup / uitab: defaults
%=============================================================================
fig = uifigure('Visible', 'off');
tg = uitabgroup(fig);
assert_isequal(class(tg), 'nelson.ui.container.TabGroup');
assert_isequal(tg.Type, 'uitabgroup');
assert_isequal(tg.TabLocation, 'top');
assert_isequal(tg.Position, [20 20 250 210]);
assert_isequal(tg.Units, 'pixels');
assert_istrue(isempty(tg.Children));
assert_istrue(isempty(tg.SelectedTab));
%=============================================================================
t1 = uitab(tg, 'Title', 'Tab One');
assert_isequal(class(t1), 'nelson.ui.container.Tab');
assert_isequal(t1.Type, 'uitab');
assert_isequal(t1.Title, 'Tab One');
assert_isequal(t1.Parent, tg);
assert_isequal(numel(tg.Children), 1);
assert_isequal(tg.SelectedTab, t1);
%=============================================================================
t2 = uitab(tg, 'Title', 'Tab Two');
assert_isequal(numel(tg.Children), 2);
% first tab remains selected
assert_isequal(tg.SelectedTab, t1);
% select second tab programmatically
tg.SelectedTab = t2;
assert_isequal(tg.SelectedTab, t2);
%=============================================================================
% components inside a tab
%=============================================================================
btn = uibutton(t1, 'Text', 'in tab');
assert_isequal(btn.Parent, t1);
assert_isequal(numel(t1.Children), 1);
%=============================================================================
% tab location
%=============================================================================
tg.TabLocation = 'bottom';
assert_isequal(tg.TabLocation, 'bottom');
delete(fig);
%=============================================================================
% tab public geometry must remain aligned with the tab group, even when the
% native page widget is smaller than the tab group because of the tab header.
%=============================================================================
fig = uifigure('Visible', 'on', 'Position', [100 100 360 260], 'Color', [1 1 1]);
fig.AutoResizeChildren = 'off';
tg = uitabgroup(fig, 'Position', [40 35 260 170]);
t = uitab(tg, 'Title', 'Stats');
lbl = uilabel(t, 'Text', 'Top text fully visible', ...
  'Position', [10 139 210 22], 'BackgroundColor', [1 1 0], ...
  'FontColor', [0 0 0]);
drawnow();
pause(0.1);
drawnow();
assert_isequal(t.Position, [1 1 260 170]);
assert_isequal(t.InnerPosition, [1 1 260 170]);
assert_isequal(t.OuterPosition, [1 1 260 170]);
assert_isequal(lbl.Position, [10 139 210 22]);
frame = getframe(fig).cdata;
yellow = double(frame(:, :, 1)) > 180 & double(frame(:, :, 2)) > 180 & ...
  double(frame(:, :, 3)) < 80;
assert_istrue(nnz(yellow) > 2500);
% Resizing and moving the header must not expose native page insets.
tg.Position = [40 35 280 190];
for location = {'top', 'bottom', 'left', 'right'}
  tg.TabLocation = location{1};
  drawnow();
  assert_isequal(t.Position, [1 1 280 190]);
  assert_isequal(t.InnerPosition, t.Position);
  assert_isequal(t.OuterPosition, t.Position);
end
delete(fig);
%=============================================================================
% normalized tab group units still produce a pixel-sized public tab area.
%=============================================================================
fig = uifigure('Visible', 'on', 'Position', [100 100 360 260], 'Color', [1 1 1]);
fig.AutoResizeChildren = 'off';
tg = uitabgroup(fig, 'Units', 'normalized', 'Position', [0.1 0.1 0.8 0.6]);
t = uitab(tg, 'Title', 'Norm');
drawnow();
pause(0.1);
drawnow();
assert_isequal(t.Position, [1 1 288 156]);
assert_isequal(t.InnerPosition, [1 1 288 156]);
assert_isequal(t.OuterPosition, [1 1 288 156]);
delete(fig);
%=============================================================================
% implicit tab group when parenting a tab to a figure
%=============================================================================
fig = uifigure('Visible', 'off');
t = uitab(fig, 'Title', 'Auto');
assert_isequal(class(t.Parent), 'nelson.ui.container.TabGroup');
assert_isequal(t.Parent.Parent, fig);
delete(fig);
%=============================================================================
close all
%=============================================================================
