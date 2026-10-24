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
% uibuttongroup: defaults (App Designer container)
%=============================================================================
fig = uifigure('Visible', 'off');
bg = uibuttongroup(fig);
assert_isequal(class(bg), 'nelson.ui.container.ButtonGroup');
assert_isequal(bg.Type, 'uibuttongroup');
assert_isequal(bg.Title, '');
assert_isequal(bg.TitlePosition, 'lefttop');
assert_isequal(bg.Position, [20 20 260 210]);
assert_isequal(bg.BorderType, 'line');
assert_isequal(bg.Units, 'pixels');
assert_isequal(char(bg.Visible), 'on');
assert_isequal(bg.Parent, fig);
assert_istrue(isempty(bg.Children));
assert_istrue(isempty(bg.SelectedObject));
assert_istrue(isempty(bg.Buttons));
%=============================================================================
% set/get
%=============================================================================
bg.Title = 'Choices';
assert_isequal(bg.Title, 'Choices');
bg.Position = [10 10 200 150];
assert_isequal(bg.Position, [10 10 200 150]);
%=============================================================================
% a non-button component can live in a button group
%=============================================================================
lbl = uilabel(bg, 'Text', 'info');
assert_isequal(lbl.Parent, bg);
delete(fig);
close all
%=============================================================================
