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
% web figure: uicontrols of a uitab sit in the tab page, under the tab bar,
% with the font size, weight and alignment of the native widgets
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
tg = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
t = uitab(tg, 'Title', 'Day');
uitab(tg, 'Title', 'Week');
b = uicontrol(t, 'Style', 'pushbutton', 'String', 'Zoom +', 'Units', 'normalized', ...
  'Position', [0.05 0.9 0.1 0.05], 'Tooltip', 'zoom in');
uicontrol(t, 'Style', 'text', 'String', 'period', 'Units', 'normalized', ...
  'Position', [0.3 0.9 0.4 0.05], 'FontWeight', 'bold', 'HorizontalAlignment', 'left');
uicontrol(t, 'Style', 'edit', 'String', 'note', 'Units', 'pixels', ...
  'Position', [10 10 100 20], 'FontUnits', 'pixels', 'FontSize', 14, 'FontAngle', 'italic');
drawnow();
%=============================================================================
dl = jsondecode(__web_display_list__(f));
group = find_control(dl, 'style', 'tabgroup');
bar = group.data.tabBarSize;
assert_istrue(bar > 0);
fh = f.Position(4);
fw = f.Position(3);
c = find_control(dl, 'text', 'Zoom +');
% the page starts after the tab bar, as the native tab page widget does
assert_isapprox([c.x, c.w, c.h], [0.05 * fw, 0.1 * fw, 0.05 * fh], 1e-9);
assert_isapprox(c.y, bar + fh - 0.95 * fh, 1e-9);
assert_istrue(c.y >= group.y + bar);
assert_isequal(c.tooltip, 'zoom in');
% FontUnits 'points' is drawn in page pixels (96 dpi)
assert_isapprox(c.fontSize, b.FontSize * 96 / 72, 1e-9);
assert_isequal(c.hAlign, 'center');
%=============================================================================
label = find_control(dl, 'text', 'period');
assert_istrue(label.bold);
assert_isequal(label.hAlign, 'left');
assert_isfalse(isfield(label, 'italic'));
%=============================================================================
edit = find_control(dl, 'text', 'note');
assert_isapprox(edit.fontSize, 14, 1e-9);
assert_istrue(edit.italic);
% pixel Position is one-based
assert_isapprox(edit.y, bar + fh - (10 - 1) - 20, 1e-9);
%=============================================================================
% a bottom tab bar leaves the page at the top of the group
tg.TabLocation = 'bottom';
drawnow();
dl = jsondecode(__web_display_list__(f));
c = find_control(dl, 'text', 'Zoom +');
assert_isapprox(c.y, fh - 0.95 * fh, 1e-9);
% a left one shifts the page to the right
tg.TabLocation = 'left';
drawnow();
dl = jsondecode(__web_display_list__(f));
bar = find_control(dl, 'style', 'tabgroup').data.tabBarSize;
c = find_control(dl, 'text', 'Zoom +');
assert_isapprox(c.x, bar + 0.05 * fw, 1e-9);
close(f);
%=============================================================================
% web mouse on an axes of a tab: callbacks order, SelectionType and CurrentPoint
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
tg = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
t = uitab(tg, 'Title', 'Day');
ax = axes('Parent', t, 'Position', [0.1 0.1 0.8 0.6], 'XLim', [0 10], 'YLim', [0 100]);
disableDefaultInteractivity(ax);
drawnow();
global UITAB_MOUSE_LOG
UITAB_MOUSE_LOG = {};
f.WindowButtonDownFcn = @(s, e) log_mouse(s, ax, 'down');
f.WindowButtonMotionFcn = @(s, e) log_mouse(s, ax, 'motion');
f.WindowButtonUpFcn = @(s, e) log_mouse(s, ax, 'up');
dl = jsondecode(__web_display_list__(f));
page = f.Position(4) - find_control(dl, 'style', 'tabgroup').data.tabBarSize;
center = [0.5 * f.Position(3), 0.4 * page];
% callbacks run between events, as in the window
events = {'press', [0 0], 'normal'; 'motion', [8 0], 'normal'; 'release', [8 0], 'normal'; ...
  'press', [0 0], 'open'; 'release', [0 0], 'open'};
for k = 1:size(events, 1)
  __figure_mouse_event__(f, events{k, 1}, center + events{k, 2}, 'web', events{k, 3});
  drawnow();
  pause(0.05);
end
kinds = cellfun(@(e) e{1}, UITAB_MOUSE_LOG, 'UniformOutput', false);
assert_isequal(kinds, {'down', 'motion', 'up', 'down', 'up'});
assert_isequal(UITAB_MOUSE_LOG{1}{2}, 'normal');
assert_isequal(UITAB_MOUSE_LOG{4}{2}, 'open');
cp = UITAB_MOUSE_LOG{1}{3};
assert_isapprox(cp(1, 1:2), [5 50], 0.05);
cp = UITAB_MOUSE_LOG{2}{3};
assert_istrue(cp(1, 1) > 5);
close(f);
clear global UITAB_MOUSE_LOG
%=============================================================================
function log_mouse(fig, ax, kind)
  global UITAB_MOUSE_LOG
  UITAB_MOUSE_LOG{end + 1} = {kind, fig.SelectionType, ax.CurrentPoint};
end
%=============================================================================
function c = find_control(dl, field, value)
  controls = dl.controls;
  if ~iscell(controls)
    controls = num2cell(controls);
  end
  for k = 1:numel(controls)
    if isfield(controls{k}, field) && strcmp(controls{k}.(field), value)
      c = controls{k};
      return
    end
  end
  error('control not found: %s', value);
end
%=============================================================================
