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
% uitogglebutton is described to the web figure as a toggle push button.
f = uifigure('Visible', 'off', 'Position', [100 120 400 300]);
bg = uibuttongroup(f, 'Position', [10 10 300 200]);
t1 = uitogglebutton(bg, 'Text', 'one', 'Position', [10 10 100 30]);
t2 = uitogglebutton(bg, 'Text', 'two', 'Position', [10 50 100 30], ...
  'HorizontalAlignment', 'left', 'VerticalAlignment', 'top');
drawnow();
dl = jsondecode(__web_display_list__(f));
c1 = find_control(dl, 'one');
c2 = find_control(dl, 'two');
assert_isequal(c1.style, 'togglebutton');
assert_isequal(c2.style, 'togglebutton');
assert_isequal(c1.text, 'one');
assert_isequal(c1.value, 1);
assert_isequal(c2.value, 0);
assert_isequal(c2.hAlign, 'left');
assert_isequal(c2.vAlign, 'top');
close(f);
%=============================================================================
% A browser click on a toggle selects it exclusively inside its group.
f = uifigure('Visible', 'off');
bg = uibuttongroup(f);
t1 = uitogglebutton(bg, 'Text', 'one');
t2 = uitogglebutton(bg, 'Text', 'two', 'Position', [10 50 100 30]);
bg.SelectionChangedFcn = @(src, evt) set(src, 'UserData', evt.NewValue.Text);
drawnow();
assert_istrue(__web_uicontrol_action__(t2, 1));
drawnow();
assert_isfalse(t1.Value);
assert_istrue(t2.Value);
assert_isequal(bg.SelectedObject, t2);
assert_isequal(bg.UserData, 'two');
% clicking the selected toggle sends 0: it must stay selected
bg.UserData = [];
assert_istrue(__web_uicontrol_action__(t2, 0));
drawnow();
assert_isfalse(t1.Value);
assert_istrue(t2.Value);
assert_isequal(bg.SelectedObject, t2);
assert_isequal(bg.UserData, []);
close(f);
%=============================================================================
% Same exclusive behaviour for radio buttons.
f = uifigure('Visible', 'off');
bg = uibuttongroup(f);
r1 = uiradiobutton(bg, 'Text', 'one');
r2 = uiradiobutton(bg, 'Text', 'two', 'Position', [10 50 100 22]);
drawnow();
assert_istrue(__web_uicontrol_action__(r2, 1));
assert_isfalse(r1.Value);
assert_istrue(r2.Value);
assert_isequal(bg.SelectedObject, r2);
close(f);
%=============================================================================
function c = find_control(dl, text)
  c = [];
  controls = dl.controls;
  for k = 1:numel(controls)
    if iscell(controls)
      item = controls{k};
    else
      item = controls(k);
    end
    if strcmp(item.text, text)
      c = item;
      return
    end
  end
  error(['Control not found in web display list: ', text]);
end
%=============================================================================
