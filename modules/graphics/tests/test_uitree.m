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
tr = uitree(fig);
assert_isequal(class(tr), 'nelson.ui.container.Tree');
assert_isequal(tr.Type, 'uitree');
assert_isequal(char(tr.Multiselect), 'off');
assert_isequal(char(tr.Editable), 'off');
assert_isequal(tr.Position, [20 20 150 300]);
assert_istrue(isempty(tr.SelectedNodes));
assert_istrue(isempty(tr.Children));
%=============================================================================
n1 = uitreenode(tr, 'Text', 'Fruits');
assert_isequal(class(n1), 'nelson.ui.container.TreeNode');
assert_isequal(n1.Type, 'uitreenode');
assert_isequal(n1.Text, 'Fruits');
assert_isequal(n1.Parent, tr);
assert_isequal(numel(tr.Children), 1);
%=============================================================================
n11 = uitreenode(n1, 'Text', 'Apple', 'NodeData', 42);
assert_isequal(n11.Parent, n1);
assert_isequal(n11.NodeData, 42);
assert_isequal(numel(n1.Children), 1);
n12 = uitreenode(n1, 'Text', 'Banana');
assert_isequal(numel(n1.Children), 2);
%=============================================================================
n1.Text = 'Best Fruits';
assert_isequal(n1.Text, 'Best Fruits');
expand(tr);
collapse(tr);
%=============================================================================
% checkbox tree
ct = uitree(fig, 'checkbox');
assert_isequal(class(ct), 'nelson.ui.container.CheckBoxTree');
assert_isequal(ct.Type, 'uicheckboxtree');
assert_istrue(isempty(ct.CheckedNodes));
c1 = uitreenode(ct, 'Text', 'Options');
c2 = uitreenode(c1, 'Text', 'Option A');
ct.CheckedNodes = c2;
assert_isequal(ct.CheckedNodes, c2);
%=============================================================================
% default text
n = uitreenode(tr);
assert_isequal(n.Text, 'Tree Node');
delete(fig);
close all
%=============================================================================
