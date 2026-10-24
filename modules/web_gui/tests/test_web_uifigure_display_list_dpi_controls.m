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
% <--WEB GUI REQUIRED--> run with: nelson-webview --file runtests_web.m
%=============================================================================
f = uifigure('Visible', 'off', 'Position', [100 120 630 420]);
drawnow();
ratio = f.DevicePixelRatio;
if ratio <= 1.01
  close(f);
  return
end
root = uipanel(f, 'Position', [0 0 630 420] * ratio, 'BorderType', 'none');
b = uibutton(root, 'Text', 'inside', 'Position', [60 40 120 30] * ratio);
lbl = uilabel(root, 'Text', 'norm', 'Units', 'normalized', ...
  'Position', [0.1 0.2 0.5 0.25]);
drawnow();
dl = jsondecode(__web_display_list__(f));
assert_isequal(dl.controlLayoutWidth, 630);
assert_isequal(dl.controlLayoutHeight, 420);
cp = find_control(dl, 'panel', '');
cb = find_control(dl, 'pushbutton', 'inside');
cl = find_control(dl, 'text', 'norm');
assert_vec_close([cp.x cp.y cp.w cp.h], [0 0 630 420], 1e-9);
assert_vec_close([cb.x cb.y cb.w cb.h], [60 350 120 30], 1e-9);
assert_vec_close([cl.x cl.y cl.w cl.h], [63 231 315 105], 1e-9);
close(f);
%=============================================================================
function assert_vec_close(actual, expected, tolerance)
  assert_isequal(size(actual), size(expected));
  assert_istrue(all(abs(actual - expected) <= tolerance));
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
