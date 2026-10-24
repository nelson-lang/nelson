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
% uigridlayout: defaults
%=============================================================================
fig = uifigure('Visible', 'off');
g = uigridlayout(fig);
assert_isequal(class(g), 'nelson.ui.container.GridLayout');
assert_isequal(g.Type, 'uigridlayout');
assert_isequal(g.RowHeight, {'1x', '1x'});
assert_isequal(g.ColumnWidth, {'1x', '1x'});
assert_isequal(g.RowSpacing, 10);
assert_isequal(g.ColumnSpacing, 10);
assert_isequal(g.Padding, [10 10 10 10]);
assert_isequal(char(g.Scrollable), 'off');
assert_isequal(char(g.Visible), 'on');
assert_isequal(g.Parent, fig);
assert_istrue(isempty(g.Children));
delete(fig);
%=============================================================================
% size argument
%=============================================================================
fig = uifigure('Visible', 'off');
g = uigridlayout(fig, [3 2]);
assert_isequal(g.RowHeight, {'1x', '1x', '1x'});
assert_isequal(g.ColumnWidth, {'1x', '1x'});
%=============================================================================
% auto-placement of children + Layout options
%=============================================================================
b1 = uibutton(g, 'Text', 'b1');
b2 = uibutton(g, 'Text', 'b2');
b3 = uibutton(g, 'Text', 'b3');
assert_isequal(numel(g.Children), 3);
assert_isequal(b1.Layout.Row, 1);
assert_isequal(b1.Layout.Column, 1);
assert_isequal(b2.Layout.Row, 1);
assert_isequal(b2.Layout.Column, 2);
assert_isequal(b3.Layout.Row, 2);
assert_isequal(b3.Layout.Column, 1);
%=============================================================================
% explicit placement and spanning
%=============================================================================
b3.Layout.Row = 3;
b3.Layout.Column = [1 2];
assert_isequal(b3.Layout.Row, 3);
assert_isequal(b3.Layout.Column, [1 2]);
%=============================================================================
% RowHeight mixing fixed / weight
%=============================================================================
g.RowHeight = {22, '1x', '2x'};
assert_isequal(g.RowHeight, {22, '1x', '2x'});
g.ColumnSpacing = 5;
assert_isequal(g.ColumnSpacing, 5);
delete(fig);
%=============================================================================
% nested grids
%=============================================================================
fig = uifigure('Visible', 'off');
g = uigridlayout(fig, [1 2]);
inner = uigridlayout(g, [2 1]);
assert_isequal(inner.Parent, g);
assert_isequal(inner.Layout.Row, 1);
assert_isequal(inner.Layout.Column, 1);
b = uibutton(inner);
assert_isequal(b.Parent, inner);
delete(fig);
close all
%=============================================================================
