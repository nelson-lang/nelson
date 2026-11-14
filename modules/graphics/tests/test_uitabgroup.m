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
% the label sits near the top of the tab page (145 pixels tall): its children
% are laid out in the page, not the whole group, so it must fit within it
lbl = uilabel(t, 'Text', 'Top text fully visible', ...
  'Position', [10 118 210 22], 'BackgroundColor', [1 1 0], ...
  'FontColor', [0 0 0]);
drawnow();
pause(0.1);
drawnow();
% a tab reports its page: the group less a 1 pixel border and a 23 pixel bar
assert_isequal(t.Position, [1 1 258 145]);
assert_isequal(t.InnerPosition, [1 1 258 145]);
assert_isequal(t.OuterPosition, [1 1 258 145]);
assert_isequal(lbl.Position, [10 118 210 22]);
frame = getframe(fig).cdata;
yellow = double(frame(:, :, 1)) > 180 & double(frame(:, :, 2)) > 180 & ...
  double(frame(:, :, 3)) < 80;
assert_istrue(nnz(yellow) > 2500);
% Resizing and moving the header must not expose native page insets.
tg.Position = [40 35 280 190];
% a uifigure tab reports its new size once its deferred resize event has run
pause(0.1);
% a side bar of a short title is 66 pixels wide
pages = {[1 1 278 165], [1 1 278 165], [1 1 212 188], [1 1 212 188]};
locations = {'top', 'bottom', 'left', 'right'};
for k = 1:numel(locations)
  tg.TabLocation = locations{k};
  drawnow();
  assert_isequal(t.Position, pages{k});
  assert_isequal(t.InnerPosition, t.Position);
  assert_isequal(t.OuterPosition, t.Position);
end
delete(fig);
%=============================================================================
% the page starts after a one pixel border, and after the 23 pixel bar when it is at the bottom
%=============================================================================
fig = uifigure('Visible', 'on', 'Position', [100 100 360 260], 'Color', [1 1 1]);
fig.AutoResizeChildren = 'off';
tg = uitabgroup(fig, 'Position', [40 35 260 170]);
t = uitab(tg, 'Title', 'Stats', 'BackgroundColor', [1 0 0]);
corner = uipanel(t, 'Position', [1 1 30 20]);
drawnow();
assert_isequal(getpixelposition(corner, true), [41 36 30 20]);
% on screen the page is 258 x 145 pixels, within the pixel a scaled display may round away
frame = getframe(fig).cdata;
red = frame(:, :, 1) > 200 & frame(:, :, 2) < 60 & frame(:, :, 3) < 60;
assert_istrue(abs(max(sum(red, 2)) - 258) <= 1);
assert_istrue(abs(max(sum(red, 1)) - 145) <= 1);
tg.TabLocation = 'bottom';
drawnow();
assert_isequal(getpixelposition(corner, true), [41 59 30 20]);
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
assert_isequal(t.Position, [1 1 286 131]);
assert_isequal(t.InnerPosition, [1 1 286 131]);
assert_isequal(t.OuterPosition, [1 1 286 131]);
delete(fig);
%=============================================================================
% children of a tab are laid out in the tab page, not the whole group: a
% normalized child is a fraction of the page and sits at the page corner, a
% pixel child is one-based in the page (reference R2025a). Tabs on top and at
% the bottom, in a classic figure and a uifigure.
%=============================================================================
for host = 1:2
  if host == 1
    fig = figure('Visible', 'off', 'Position', [100 100 700 500]);
    tg = uitabgroup(fig, 'Units', 'pixels', 'Position', [10 10 200 150]);
  else
    fig = uifigure('Visible', 'off', 'Position', [100 100 700 500]);
    fig.AutoResizeChildren = 'off';
    tg = uitabgroup(fig, 'Position', [10 10 200 150]);
  end
  for location = {'top', 'bottom'}
    tg.TabLocation = location{1};
    t = uitab(tg, 'Title', 'a');
    % the tab fills the group, its children fill the page (198 x 125)
    full = uipanel(t, 'Units', 'normalized', 'Position', [0 0 1 1]);
    assert_isequal(getpixelposition(full), [1 1 198 125]);
    quarter = uicontrol(t, 'Units', 'normalized', 'Position', [0.25 0.25 0.5 0.5]);
    assert_isequal(getpixelposition(quarter), [50.5 32.25 99 62.5]);
    pixelChild = uipanel(t, 'Units', 'pixels', 'Position', [1 1 50 30]);
    assert_isequal(getpixelposition(pixelChild), [1 1 50 30]);
    % a child at the page corner coincides with the tab (recursive) corner
    assert_isequal(getpixelposition(pixelChild, true), getpixelposition(t, true));
    delete(t);
  end
  delete(fig);
end
%=============================================================================
% a tab needs a tab group parent
%=============================================================================
fig = uifigure('Visible', 'off');
msg = _('Parent must be a TabGroup object.');
assert_checkerror('uitab(fig, ''Title'', ''Auto'')', msg, 'Nelson:uicomponent:parentMustBeTabGroupObject');
delete(fig);
%=============================================================================
close all
%=============================================================================
