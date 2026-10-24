%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--WEB GUI REQUIRED-->
% run with: nelson-webview --file runtests_web.m
%=============================================================================
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 120 420 310]);
assert_vec_close(f.Position, f.InnerPosition, 1e-9);
f.InnerPosition = [110 130 430 320];
drawnow();
assert_vec_close(f.Position, [110 130 430 320], 1e-9);
assert_vec_close(f.Position, f.InnerPosition, 1e-9);
f.OuterPosition = [120 140 440 330];
drawnow();
assert_vec_close(f.Position, f.InnerPosition, 1e-9);
assert_vec_close(f.OuterPosition, [120 140 440 330], 3);
%=============================================================================
ax = axes('Parent', f);
ax.InnerPosition = [0.25 0.25 0.55 0.45];
drawnow();
assert_isequal(ax.PositionConstraint, 'innerposition');
assert_vec_close(ax.Position, ax.InnerPosition, 1e-9);
ax.OuterPosition = [0.1 0.1 0.8 0.75];
drawnow();
assert_isequal(ax.PositionConstraint, 'outerposition');
assert_vec_close(ax.OuterPosition, [0.1 0.1 0.8 0.75], 1e-9);
assert_vec_close(ax.Position, ax.InnerPosition, 1e-9);
%=============================================================================
u = uicontrol('Parent', f, 'Style', 'text', 'String', 'web', 'Position', [30 40 120 30]);
u.InnerPosition = [35 45 130 35];
drawnow();
assert_vec_close(u.Position, [35 45 130 35], 1e-9);
assert_vec_close(u.Position, u.InnerPosition, 1e-9);
assert_vec_close(u.Position, u.OuterPosition, 1e-9);
u.OuterPosition = [25 35 140 45];
drawnow();
assert_vec_close(u.Position, [25 35 140 45], 1e-9);
assert_vec_close(u.Position, u.InnerPosition, 1e-9);
assert_vec_close(u.Position, u.OuterPosition, 1e-9);
frame = getframe(f);
assert_istrue(size(frame.cdata, 1) > 0);
assert_istrue(size(frame.cdata, 2) > 0);
dl = jsondecode(__web_display_list__(f));
assert_isequal(dl.controlLayoutWidth, f.Position(3));
assert_isequal(dl.controlLayoutHeight, f.Position(4));
cu = find_control(dl, 'text', 'web');
assert_vec_close([cu.x cu.y cu.w cu.h], [25 250 140 45], 1e-9);
assert_frame_region_not_white(frame, cu);
%=============================================================================
uf = uifigure('Visible', 'off', 'Position', [100 120 420 310]);
p = uipanel(uf, 'Position', [30 40 200 120]);
assert_vec_close(p.Position, p.OuterPosition, 1e-9);
assert_readonly(p, 'InnerPosition');
p.OuterPosition = [25 35 210 130];
drawnow();
assert_vec_close(p.Position, [25 35 210 130], 1e-9);
assert_vec_close(p.Position, p.OuterPosition, 1e-9);
b = uibutton(p, 'Text', 'inside', 'Position', [20 20 80 25]);
b.InnerPosition = [22 24 90 28];
drawnow();
assert_vec_close(b.Position, [22 24 90 28], 1e-9);
assert_vec_close(b.Position, b.InnerPosition, 1e-9);
assert_vec_close(b.Position, b.OuterPosition, 1e-9);
lbl = uilabel(p, 'Text', 'norm', 'Units', 'normalized', 'Position', [0.1 0.2 0.5 0.25]);
drawnow();
frame = getframe(uf);
assert_istrue(size(frame.cdata, 1) > 0);
assert_istrue(size(frame.cdata, 2) > 0);
dl = jsondecode(__web_display_list__(uf));
cp = find_control(dl, 'panel', '');
cb = find_control(dl, 'pushbutton', 'inside');
cl = find_control(dl, 'text', 'norm');
assert_vec_close([cp.x cp.y cp.w cp.h], [25 145 210 130], 1e-9);
% children sit in the panel content area: inside its 1 pixel border (208 x 128)
assert_vec_close([cb.x cb.y cb.w cb.h], [48 222 90 28], 1e-9);
assert_vec_close([cl.x cl.y cl.w cl.h], [46.8 216.4 104 32], 1e-9);
assert_frame_region_not_white(frame, cb);
close('all');
%=============================================================================
function assert_vec_close(actual, expected, tolerance)
  assert_isequal(size(actual), size(expected));
  assert_istrue(all(abs(actual - expected) <= tolerance));
end
%=============================================================================
function assert_readonly(h, name)
  failed = false;
  try
    set(h, name, [1 2 3 4]);
  catch
    failed = true;
  end
  assert_istrue(failed);
end
%=============================================================================
function c = find_control(dl, style, text)
  c = [];
  controls = dl.controls;
  for k = 1:numel(controls)
    if iscell(controls)
      item = controls{k};
    else
      item = controls(k);
    end
    if strcmp(item.style, style) && strcmp(item.text, text)
      c = item;
      return
    end
  end
  error(_('Control not found in web display list.'));
end
%=============================================================================
function assert_frame_region_not_white(frame, control)
  cdata = frame.cdata;
  rows = max(1, floor(control.y) + 1):min(size(cdata, 1), ceil(control.y + control.h));
  cols = max(1, floor(control.x) + 1):min(size(cdata, 2), ceil(control.x + control.w));
  region = cdata(rows, cols, :);
  assert_istrue(mean(double(region(:))) < 252);
end
%=============================================================================
