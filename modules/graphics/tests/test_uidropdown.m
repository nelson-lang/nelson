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
dd = uidropdown(fig);
assert_isequal(class(dd), 'nelson.ui.control.DropDown');
assert_isequal(dd.Type, 'uidropdown');
assert_isequal(dd.Items, {'Option 1', 'Option 2', 'Option 3', 'Option 4'});
assert_isequal(dd.Value, 'Option 1');
assert_isequal(dd.ValueIndex, 1);
assert_isequal(dd.ItemsData, []);
assert_isequal(char(dd.Editable), 'off');
assert_isequal(dd.Placeholder, '');
assert_isequal(dd.Position, [100 100 100 22]);
%=============================================================================
% select by value
dd.Value = 'Option 3';
assert_isequal(dd.Value, 'Option 3');
assert_isequal(dd.ValueIndex, 3);
% select by index
dd.ValueIndex = 2;
assert_isequal(dd.Value, 'Option 2');
assert_isequal(dd.ValueIndex, 2);
%=============================================================================
% invalid value errors
didError = false;
try
  dd.Value = 'Nope';
catch
  didError = true;
end
assert_istrue(didError);
%=============================================================================
% ItemsData mapping
dd.Items = {'Red', 'Green', 'Blue'};
dd.ItemsData = [10 20 30];
assert_isequal(dd.Value, 10);
dd.Value = 20;
assert_isequal(dd.Value, 20);
assert_isequal(dd.ValueIndex, 2);
dd.ItemsData = [-10 0 10];
dd.Value = -10;
assert_isequal(dd.Value, -10);
assert_isequal(dd.ValueIndex, 1);
delete(fig);
close all
%=============================================================================
