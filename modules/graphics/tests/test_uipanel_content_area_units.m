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
% ui components held by a uipanel are laid out in its content area: the panel
% size minus its border and its title band
f = uifigure('Visible', 'off', 'Position', [100 100 400 300]);
p = uipanel(f, 'Units', 'pixels', 'Position', [20 20 200 100], 'BorderWidth', 5);
b = uibutton(p, 'Units', 'normalized', 'Position', [0 0 1 1]);
b.Units = 'pixels';
assert_isapprox(b.Position, [0 0 190 90], 1e-12);
b.Units = 'normalized';
assert_isapprox(b.Position, [0 0 1 1], 1e-12);
%=============================================================================
p2 = uipanel(f, 'Units', 'pixels', 'Position', [20 20 200 100], ...
  'BorderType', 'none', 'Title', 'Panel');
b2 = uibutton(p2, 'Units', 'normalized', 'Position', [0 0 1 1]);
b2.Units = 'pixels';
assert_isapprox(b2.Position(1:3), [0 0 200], 1e-12);
assert_istrue(b2.Position(4) < 100 && b2.Position(4) > 80);
%=============================================================================
% nested panels: the inner one fills the outer content area
inner = uipanel(p, 'Units', 'normalized', 'Position', [0 0 1 1], 'BorderType', 'none');
b3 = uibutton(inner, 'Units', 'normalized', 'Position', [0 0 0.5 0.5]);
b3.Units = 'pixels';
assert_isapprox(b3.Position, [0 0 95 45], 1e-12);
%=============================================================================
% web placement: a child at [0 0] sits inside the border, in figure pixels
b4 = uibutton(p, 'Text', 'corner', 'Position', [0 0 40 20]);
drawnow();
dl = jsondecode(__web_display_list__(f));
c = find_control(dl, 'corner');
assert_isequal([c.x, c.w, c.h], [25 40 20]);
assert_isequal(c.y, 300 - (20 + 5) - 20);
close(f);
%=============================================================================
function c = find_control(dl, text)
  c = [];
  controls = dl.controls;
  if ~iscell(controls)
    controls = num2cell(controls);
  end
  for k = 1:numel(controls)
    if isfield(controls{k}, 'text') && strcmp(controls{k}.text, text)
      c = controls{k};
      return
    end
  end
  error('control not found: %s', text);
end
%=============================================================================
