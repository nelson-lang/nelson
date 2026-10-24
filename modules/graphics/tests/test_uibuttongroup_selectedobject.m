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
% setting SelectedObject from code selects that button and clears the others,
% without firing SelectionChangedFcn
f = figure('Visible', 'off');
bg = uibuttongroup(f, 'Position', [0 0 1 1]);
r1 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'one', 'Position', [10 60 100 20]);
r2 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'two', 'Position', [10 30 100 20]);
r3 = uicontrol(bg, 'Style', 'togglebutton', 'String', 'three', 'Min', 2, 'Max', 7, ...
  'Value', 2, 'Position', [10 5 100 20]);
bg.SelectionChangedFcn = @(src, evt) set(src, 'UserData', 'fired');
bg.SelectedObject = r2;
drawnow();
assert_isequal(bg.SelectedObject, r2);
assert_isequal([r1.Value, r2.Value, r3.Value], [0 1 2]);
bg.SelectedObject = r3;
drawnow();
assert_isequal(bg.SelectedObject, r3);
assert_isequal([r1.Value, r2.Value, r3.Value], [0 0 7]);
assert_istrue(isempty(bg.UserData));
% a handle that is not one of the group buttons keeps the current selection
other = uicontrol(f, 'Style', 'radiobutton');
bg.SelectedObject = other;
drawnow();
assert_isequal(bg.SelectedObject, r3);
assert_isequal(other.Value, 0);
close(f);
%=============================================================================
% same for ui radio buttons
f = uifigure('Visible', 'off');
bg = uibuttongroup(f);
a = uiradiobutton(bg, 'Text', 'a');
b = uiradiobutton(bg, 'Text', 'b', 'Position', [10 50 100 22]);
bg.SelectionChangedFcn = @(src, evt) set(src, 'UserData', 'fired');
bg.SelectedObject = b;
drawnow();
assert_isequal(bg.SelectedObject, b);
assert_isfalse(a.Value);
assert_istrue(b.Value);
assert_istrue(isempty(bg.UserData));
close(f);
%=============================================================================
