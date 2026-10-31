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
fig = uifigure('Visible', 'off', 'Position', [100 100 400 300]);
%=============================================================================
btn = uibutton(fig, 'Position', [40 30 100 60]);
assert_isequal(btn.Units, 'pixels');
btn.Units = 'normalized';
assert_isequal(btn.Units, 'normalized');
% pixel positions start at 1, the other units at 0
assert_isapprox(btn.Position, [39 29 100 60] ./ [400 300 400 300], 1e-12);
assert_isapprox(btn.InnerPosition, btn.Position, 1e-12);
assert_isapprox(btn.OuterPosition, btn.Position, 1e-12);
btn.Units = 'pixels';
assert_isapprox(btn.Position, [40 30 100 60], 1e-9);
%=============================================================================
lbl = uilabel(fig, 'Units', 'normalized', 'Position', [0.2 0.3 0.4 0.1]);
assert_isequal(lbl.Units, 'normalized');
assert_isapprox(lbl.Position, [0.2 0.3 0.4 0.1], 1e-12);
lbl.Units = 'pixels';
assert_isapprox(lbl.Position, [81 91 160 30], 1e-9);
lbl.Units = 'normalized';
lbl.InnerPosition = [0.25 0.25 0.5 0.2];
drawnow();
assert_isapprox(lbl.Position, [0.25 0.25 0.5 0.2], 1e-12);
%=============================================================================
% units inside a panel are relative to its content area: the 1 pixel border
% leaves 198 x 98 of the 200 x 100 panel
panel = uipanel(fig, 'Position', [20 30 200 100]);
txt = uieditfield(panel, 'Position', [10 5 50 20]);
assert_isequal(txt.Units, 'pixels');
txt.Units = 'normalized';
assert_isapprox(txt.Position, [9 / 198 4 / 98 50 / 198 20 / 98], 1e-12);
txt.Position = [0.1 0.2 0.3 0.4];
txt.Units = 'pixels';
assert_isapprox(txt.Position, [20.8 20.6 59.4 39.2], 1e-9);
%=============================================================================
num = uieditfield(fig, 'numeric', 'Units', 'normalized', 'Position', [0.5 0.5 0.2 0.1]);
assert_isequal(num.Type, 'uinumericeditfield');
assert_isequal(num.Units, 'normalized');
num.Units = 'pixels';
assert_isapprox(num.Position, [201 151 80 30], 1e-9);
%=============================================================================
failed = false;
try
  btn.Units = 'invalid';
catch
  failed = true;
end
assert_istrue(failed);
%=============================================================================
% Units is a functional-but-hidden property on every control component: it is
% not listed by properties()/isprop (so the public surface matches the
% reference), yet get/set works. This must be CONSISTENT across a component and
% all of its style variants.
bg = uibuttongroup(fig);
controls = {uibutton(fig), uibutton(fig, 'state'), uicheckbox(fig), ...
  uidropdown(fig), uieditfield(fig), uieditfield(fig, 'numeric'), uigauge(fig), ...
  uigauge(fig, 'linear'), uiknob(fig), uiknob(fig, 'discrete'), uilabel(fig), ...
  uilamp(fig), uilistbox(fig), uislider(fig), uislider(fig, 'range'), ...
  uispinner(fig), uiswitch(fig), uiswitch(fig, 'rocker'), uiswitch(fig, 'toggle'), ...
  uitextarea(fig), uidatepicker(fig), uitree(fig), uiradiobutton(bg), ...
  uitogglebutton(bg), uigridlayout(fig)};
for k = 1:numel(controls)
  c = controls{k};
  assert_isfalse(isprop(c, 'Units'));
  assert_isfalse(any(strcmp(properties(c), 'Units')));
  assert_isequal(c.Units, 'pixels');
  c.Units = 'normalized';
  assert_isequal(c.Units, 'normalized');
end
% Legacy container components keep Units on their public surface.
containers = {uipanel(fig), uibuttongroup(fig), uitable(fig), uitabgroup(fig)};
for k = 1:numel(containers)
  assert_istrue(isprop(containers{k}, 'Units'));
end
% Radio/toggle buttons and the grid layout also hide Layout / Enable (functional
% but absent from the reference's public surface); a plain button keeps Layout.
rb = uiradiobutton(bg);
tb = uitogglebutton(bg);
gl = uigridlayout(fig);
assert_isfalse(isprop(rb, 'Layout'));
assert_isfalse(isprop(tb, 'Layout'));
assert_isfalse(isprop(gl, 'Enable'));
assert_isequal(char(gl.Enable), 'on');
assert_istrue(isprop(uibutton(fig), 'Layout'));
delete(fig);
close all
%=============================================================================
