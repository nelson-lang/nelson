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
% Runtime firing of the uifigure interaction callbacks, exercised headless with
% the __uicomponent_event__ test hook (replays a widget event through the same
% model path the Qt binding uses): uitree NodeExpanded/NodeCollapsed, uilistbox
% DoubleClicked, uitable Clicked/DoubleClicked, uidropdown DropDownOpening.
%=============================================================================
global UIFIG_CB
UIFIG_CB = {};
uf = uifigure('Visible', 'off');
g = uigridlayout(uf, [2 2]);

tree = uitree(g);
parentNode = uitreenode(tree, 'Text', 'ParentNode');
uitreenode(parentNode, 'Text', 'ChildNode');
tree.NodeExpandedFcn = @(s, e) record_cb('expand', e.Node.Text);
tree.NodeCollapsedFcn = @(s, e) record_cb('collapse', e.Node.Text);

lb = uilistbox(g, 'Items', {'alpha', 'beta', 'gamma'});
lb.DoubleClickedFcn = @(s, e) record_cb('listbox', '');

tbl = uitable(g, 'Data', magic(4));
tbl.ClickedFcn = @(s, e) record_cb('tableClick', sprintf('%d,%d', e.Row, e.Column));
tbl.DoubleClickedFcn = @(s, e) record_cb('tableDouble', sprintf('%d,%d', e.Row, e.Column));

dd = uidropdown(g, 'Items', {'x', 'y', 'z'});
dd.DropDownOpeningFcn = @(s, e) record_cb('dropdown', '');
drawnow();
%=============================================================================
% tree expand / collapse carry the toggled node
assert_istrue(__uicomponent_event__(tree, 'expand', parentNode));
assert_istrue(__uicomponent_event__(tree, 'collapse', parentNode));
% listbox double click
assert_istrue(__uicomponent_event__(lb, 'doubleclick'));
% table single and double click carry the 1-based cell
assert_istrue(__uicomponent_event__(tbl, 'click', 2, 3));
assert_istrue(__uicomponent_event__(tbl, 'doubleclick', 4, 1));
% dropdown opening
assert_istrue(__uicomponent_event__(dd, 'opening'));
drawnow();
pause(0.5);
%=============================================================================
assert_isequal(numel(UIFIG_CB), 6);
assert_isequal(UIFIG_CB{1}, {'expand', 'ParentNode'});
assert_isequal(UIFIG_CB{2}, {'collapse', 'ParentNode'});
assert_isequal(UIFIG_CB{3}, {'listbox', ''});
assert_isequal(UIFIG_CB{4}, {'tableClick', '2,3'});
assert_isequal(UIFIG_CB{5}, {'tableDouble', '4,1'});
assert_isequal(UIFIG_CB{6}, {'dropdown', ''});
%=============================================================================
clear global UIFIG_CB
%=============================================================================
function record_cb(tag, detail)
  global UIFIG_CB
  UIFIG_CB{end + 1} = {tag, detail};
end
%=============================================================================
