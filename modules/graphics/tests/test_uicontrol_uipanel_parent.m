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
skip_if_portable_graphics_backend('native uipanel layout metrics differ in portable rendering.');
% a uicontrol can be held by a uipanel, a uibuttongroup or a uitab
f = figure('Visible', 'off', 'Position', [100 100 400 300]);
p = uipanel(f, 'Units', 'pixels', 'Position', [20 20 200 100], 'BorderWidth', 5);
u = uicontrol(p, 'Style', 'pushbutton', 'String', 'in panel', ...
  'Units', 'pixels', 'Position', [10 10 60 20]);
assert_istrue(u.Parent == p);
assert_istrue(any(p.Children == u));
assert_isfalse(any(f.Children == u));
%=============================================================================
u2 = uicontrol('Parent', p, 'Style', 'text', 'String', 'by name');
assert_istrue(u2.Parent == p);
%=============================================================================
% Units are relative to the panel content area (border deducted)
u3 = uicontrol(p, 'Style', 'edit', 'Units', 'normalized', 'Position', [0 0 1 1]);
u3.Units = 'pixels';
% pixel positions start at 1
assert_isapprox(u3.Position, [1 1 190 90], 1e-12);
%=============================================================================
% web placement: offset by the panel frame and its border
drawnow();
dl = jsondecode(__web_display_list__(f));
c = find_control(dl, 'in panel');
assert_isequal([c.x, c.w, c.h], [19 + 5 + 9, 60, 20]);
assert_isequal(c.y, 300 - (19 + 5 + 9) - 20);
%=============================================================================
bg = uibuttongroup(f, 'Position', [0 0 0.5 0.5]);
r = uicontrol(bg, 'Style', 'radiobutton', 'String', 'radio');
assert_istrue(r.Parent == bg);
tg = uitabgroup(f);
t = uitab(tg, 'Title', 'Tab');
ut = uicontrol(t, 'Style', 'checkbox', 'String', 'in tab');
assert_istrue(ut.Parent == t);
%=============================================================================
% deleting the panel deletes its uicontrols
delete(p);
assert_isfalse(isgraphics(u));
assert_isfalse(isgraphics(u3));
%=============================================================================
ax = axes(f);
assert_checkerror('uicontrol(ax)', getString(message('Nelson:uicontrol:invalidParent')), 'Nelson:uicontrol:invalidParent');
assert_checkerror('uicontrol(''Parent'', ax)', getString(message('Nelson:uicontrol:expectedFigure')), 'Nelson:uicontrol:expectedFigure');
close(f);
%=============================================================================
function c = find_control(dl, text)
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
