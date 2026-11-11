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
% expand/collapse act on the given tree level or node only, 'all' recurses.
% The visible node rows are counted on the exported image.
%=============================================================================
function test_uitree_expand_collapse()
  fig = uifigure('Visible', 'off', 'Position', [100 100 300 220]);
  tr = uitree(fig, 'Position', [20 10 260 200]);
  n1 = uitreenode(tr, 'Text', 'Fruits');
  n11 = uitreenode(n1, 'Text', 'Apples');
  uitreenode(n11, 'Text', 'Gala');
  n2 = uitreenode(tr, 'Text', 'Vegetables');
  uitreenode(n2, 'Text', 'Carrot');
  tr.NodeExpandedFcn = @(src, evt) setappdata(fig, 'expandedFcnCalled', true);
  %=============================================================================
  % nodes start collapsed: only top-level rows
  assert_isequal(visibleRows(fig), 2);
  %=============================================================================
  % expand(tree) expands the top-level nodes only
  expand(tr);
  assert_isequal(visibleRows(fig), 4);
  collapse(tr);
  assert_isequal(visibleRows(fig), 2);
  %=============================================================================
  % expand(node) expands that node only
  expand(n1);
  assert_isequal(visibleRows(fig), 3);
  expand(n11);
  assert_isequal(visibleRows(fig), 4);
  collapse(n1);
  assert_isequal(visibleRows(fig), 2);
  %=============================================================================
  % collapse(node) keeps the descendant states
  expand(n1);
  assert_isequal(visibleRows(fig), 4);
  %=============================================================================
  % expand(tree, 'all') and collapse(node, 'all')
  collapse(tr, 'all');
  assert_isequal(visibleRows(fig), 2);
  expand(tr, 'all');
  assert_isequal(visibleRows(fig), 5);
  collapse(n1, 'all');
  assert_isequal(visibleRows(fig), 3);
  expand(n1);
  assert_isequal(visibleRows(fig), 4);
  collapse(n1);
  expand(n1, 'all');
  assert_isequal(visibleRows(fig), 5);
  %=============================================================================
  % collapse(tree) collapses the top-level nodes only
  collapse(tr);
  assert_isequal(visibleRows(fig), 2);
  expand(tr);
  assert_isequal(visibleRows(fig), 5);
  %=============================================================================
  % case-insensitive partial option, string scalar
  collapse(tr, 'AL');
  assert_isequal(visibleRows(fig), 2);
  expand(n2, "all");
  assert_isequal(visibleRows(fig), 3);
  collapse(tr, 'a');
  %=============================================================================
  % programmatic changes do not fire NodeExpandedFcn
  drawnow();
  assert_isfalse(isappdata(fig, 'expandedFcnCalled'));
  %=============================================================================
  % new nodes are collapsed
  uitreenode(n2, 'Text', 'Leek');
  expand(n2);
  assert_isequal(visibleRows(fig), 4);
  %=============================================================================
  % web frontend toggles update the node and fire the node callbacks
  tr.NodeExpandedFcn = @(src, evt) setappdata(fig, 'expandedNode', evt.Node.Text);
  tr.NodeCollapsedFcn = @(src, evt) setappdata(fig, 'collapsedNode', evt.Node.Text);
  [fruitsId, fruitsExpanded] = webNode(fig, 'Fruits');
  assert_isfalse(fruitsExpanded);
  assert_istrue(__web_uicontrol_action__(tr, 1, fruitsId));
  drawnow();
  assert_isequal(getappdata(fig, 'expandedNode'), 'Fruits');
  assert_isequal(visibleRows(fig), 5);
  [~, fruitsExpanded] = webNode(fig, 'Fruits');
  assert_istrue(fruitsExpanded);
  assert_istrue(__web_uicontrol_action__(tr, -1, fruitsId));
  drawnow();
  assert_isequal(getappdata(fig, 'collapsedNode'), 'Fruits');
  assert_isequal(visibleRows(fig), 4);
  % no change, no callback
  rmappdata(fig, 'collapsedNode');
  assert_istrue(__web_uicontrol_action__(tr, -1, fruitsId));
  drawnow();
  assert_isfalse(isappdata(fig, 'collapsedNode'));
  %=============================================================================
  % invalid inputs
  assert_checkerror('expand(tr, ''foo'')', _('Second argument must be ''all''.'));
  assert_checkerror('collapse(tr, 1)', _('Second argument must be ''all''.'));
  assert_checkerror('expand(tr, '''')', _('Second argument must be ''all''.'));
  assert_checkerror('expand(fig)', _('Tree or TreeNode graphics object expected.'));
  assert_checkerror('collapse(3)', _('Tree or TreeNode graphics object expected.'));
  assert_checkerror('expand(tr, ''all'', 1)', _('Too many input arguments.'));
  delete(fig);
end
%=============================================================================
function n = visibleRows(fig)
  drawnow();
  pngFile = [tempname(), '.png'];
  exportapp(fig, pngFile);
  img = imread(pngFile);
  rmfile(pngFile);
  dark = (img(:, :, 1) < 100) & (img(:, :, 2) < 100) & (img(:, :, 3) < 100);
  rows = any(dark, 2);
  n = sum(diff([false; rows(:)]) == 1);
end
%=============================================================================
function [id, expanded] = webNode(fig, text)
  % node handle (as sent by the frontend) and expanded flag from the display list
  displayList = jsondecode(__web_display_list__(fig));
  controls = displayList.controls;
  if ~iscell(controls)
    controls = num2cell(controls);
  end
  nodes = {};
  for k = 1:numel(controls)
    if strcmp(controls{k}.style, 'tree')
      nodes = controls{k}.data.nodes;
    end
  end
  if ~iscell(nodes)
    nodes = num2cell(nodes);
  end
  for k = 1:numel(nodes)
    if strcmp(nodes{k}.text, text)
      id = sprintf('%d', nodes{k}.handle);
      expanded = nodes{k}.expanded;
      return
    end
  end
  error('node not found');
end
%=============================================================================
