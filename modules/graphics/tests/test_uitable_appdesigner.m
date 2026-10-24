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
t = uitable(fig);
assert_isequal(class(t), 'nelson.ui.control.Table');
assert_isequal(t.Type, 'uitable');
assert_isequal(t.Data, []);
assert_isequal(t.ColumnName, 'numbered');
assert_isequal(t.RowName, 'numbered');
assert_isequal(t.ColumnWidth, 'auto');
assert_isequal(t.Position, [20 20 300 300]);
assert_isequal(t.SelectionType, 'cell');
assert_isequal(char(t.Multiselect), 'on');
assert_isequal(char(t.RowStriping), 'on');
%=============================================================================
t.Data = magic(4);
assert_isequal(t.Data, magic(4));
assert_isequal(t.DisplayData, magic(4));
t.ColumnName = {'A', 'B', 'C', 'D'};
assert_isequal(t.ColumnName, {'A', 'B', 'C', 'D'});
t.ColumnEditable = [true false true false];
assert_isequal(t.ColumnEditable, [true false true false]);
%=============================================================================
% cell data
t2 = uitable(fig, 'Data', {1, 'two'; 3, 'four'});
assert_isequal(t2.Data, {1, 'two'; 3, 'four'});
delete(fig);
close all
%=============================================================================
