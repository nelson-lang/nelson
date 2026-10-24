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
lb = uilistbox(fig);
assert_isequal(class(lb), 'nelson.ui.control.ListBox');
assert_isequal(lb.Type, 'uilistbox');
assert_isequal(lb.Items, {'Item 1', 'Item 2', 'Item 3', 'Item 4'});
assert_isequal(lb.Value, 'Item 1');
assert_isequal(lb.ValueIndex, 1);
assert_isequal(char(lb.Multiselect), 'off');
assert_isequal(lb.Position, [100 100 100 74]);
assert_isequal(lb.BackgroundColor, [1 1 1]);
assert_istrue(isprop(lb, 'StyleConfigurations'));
%=============================================================================
lb.Value = 'Item 3';
assert_isequal(lb.Value, 'Item 3');
assert_isequal(lb.ValueIndex, 3);
%=============================================================================
% multiselect with cell values
lb.Multiselect = 'on';
lb.Value = {'Item 1', 'Item 4'};
assert_isequal(lb.ValueIndex, [1 4]);
%=============================================================================
% ItemsData
lb2 = uilistbox(fig, 'Items', {'a', 'b'}, 'ItemsData', {'A', 'B'});
assert_isequal(lb2.Value, 'A');
lb2.Value = 'B';
assert_isequal(lb2.ValueIndex, 2);
lb2.ItemsData = [-1 1];
lb2.Value = -1;
assert_isequal(lb2.Value, -1);
assert_isequal(lb2.ValueIndex, 1);
delete(fig);
close all
%=============================================================================
