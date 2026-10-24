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
% a uibuttongroup manages its uicontrol radio buttons exclusively
f = figure('Visible', 'off');
bg = uibuttongroup(f, 'Position', [0 0 1 1]);
r1 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'one', 'Position', [10 60 100 20]);
r2 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'two', 'Position', [10 30 100 20]);
r3 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'three', 'Position', [10 5 100 20]);
% the first button is selected
assert_isequal(bg.SelectedObject, r1);
assert_isequal([r1.Value, r2.Value, r3.Value], [1 0 0]);
%=============================================================================
% a click selects the button, clears the previous one and fires the event;
% the group reports it: the Callback of the button itself does not run
bg.SelectionChangedFcn = @(src, evt) set(src, 'UserData', [evt.OldValue, evt.NewValue]);
r2.Callback = @(src, evt) set(f, 'UserData', 'button callback');
assert_istrue(__web_uicontrol_action__(r2, 1));
% each drawnow runs one queued callback: drain them all
for k = 1:3
  drawnow();
end
assert_isequal([r1.Value, r2.Value, r3.Value], [0 1 0]);
assert_isequal(bg.SelectedObject, r2);
assert_isequal(bg.UserData, [r1, r2]);
assert_istrue(isempty(f.UserData));
%=============================================================================
% clicking the selected button keeps it selected, without event
bg.UserData = [];
assert_istrue(__web_uicontrol_action__(r2, 0));
drawnow();
assert_isequal([r1.Value, r2.Value, r3.Value], [0 1 0]);
assert_isequal(bg.SelectedObject, r2);
assert_istrue(isempty(bg.UserData));
%=============================================================================
% Value set to Max from code selects the button, without event
r3.Value = r3.Max;
drawnow();
assert_isequal([r1.Value, r2.Value, r3.Value], [0 0 1]);
assert_isequal(bg.SelectedObject, r3);
assert_istrue(isempty(bg.UserData));
%=============================================================================
% Min and Max are honoured
r4 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'four', 'Min', -1, 'Max', 5, 'Value', -1);
assert_isequal(bg.SelectedObject, r3);
assert_istrue(__web_uicontrol_action__(r4, 5));
drawnow();
assert_isequal([r3.Value, r4.Value], [0 5]);
assert_isequal(bg.SelectedObject, r4);
close(f);
%=============================================================================
% toggle buttons behave the same
f = figure('Visible', 'off');
bg = uibuttongroup(f, 'Position', [0 0 1 1]);
t1 = uicontrol(bg, 'Style', 'togglebutton', 'String', 'A', 'Position', [10 10 60 20]);
t2 = uicontrol(bg, 'Style', 'togglebutton', 'String', 'B', 'Position', [80 10 60 20]);
assert_isequal([t1.Value, t2.Value], [1 0]);
assert_istrue(__web_uicontrol_action__(t2, 1));
drawnow();
assert_isequal([t1.Value, t2.Value], [0 1]);
assert_isequal(bg.SelectedObject, t2);
%=============================================================================
% radio buttons outside a button group stay independent and run their Callback
p = uipanel(f);
a = uicontrol(p, 'Style', 'radiobutton', 'String', 'a');
b = uicontrol(p, 'Style', 'radiobutton', 'String', 'b');
a.Value = 1;
b.Value = 1;
drawnow();
assert_isequal([a.Value, b.Value], [1 1]);
b.Callback = @(src, evt) set(p, 'UserData', 'b callback');
assert_istrue(__web_uicontrol_action__(b, 0));
drawnow();
assert_isequal(p.UserData, 'b callback');
close(f);
%=============================================================================
