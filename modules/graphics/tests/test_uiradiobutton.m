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
fig = uifigure('Visible', 'off');
bg = uibuttongroup(fig);
rb1 = uiradiobutton(bg);
assert_isequal(class(rb1), 'nelson.ui.control.RadioButton');
assert_isequal(rb1.Type, 'uiradiobutton');
assert_isequal(rb1.Text, 'Radio Button');
assert_isequal(rb1.Position, [10 10 91 22]);
% first button of the group is selected
assert_isequal(rb1.Value, true);
assert_isequal(bg.SelectedObject, rb1);
assert_isequal(numel(bg.Buttons), 1);
%=============================================================================
rb2 = uiradiobutton(bg, 'Text', 'Second');
assert_isequal(rb2.Value, false);
assert_isequal(bg.SelectedObject, rb1);
assert_isequal(numel(bg.Buttons), 2);
%=============================================================================
% programmatic selection moves the group selection silently
rb2.Value = true;
assert_isequal(rb2.Value, true);
assert_isequal(rb1.Value, false);
assert_isequal(bg.SelectedObject, rb2);
%=============================================================================
% toggle buttons in their own group (mixing types is not allowed)
bg2 = uibuttongroup(fig);
tb1 = uitogglebutton(bg2);
assert_isequal(class(tb1), 'nelson.ui.control.ToggleButton');
assert_isequal(tb1.Type, 'uitogglebutton');
assert_isequal(tb1.Text, 'Toggle Button');
assert_isequal(tb1.Value, true);
tb2 = uitogglebutton(bg2, 'Text', 'T2');
assert_isequal(tb2.Value, false);
tb2.Value = true;
assert_isequal(tb1.Value, false);
assert_isequal(bg2.SelectedObject, tb2);
%=============================================================================
% radio button requires a button group parent
didError = false;
try
  uiradiobutton(fig);
catch
  didError = true;
end
assert_istrue(didError);
delete(fig);
close all
%=============================================================================
