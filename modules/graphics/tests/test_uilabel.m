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
% uilabel: defaults
%=============================================================================
fig = uifigure('Visible', 'off');
lbl = uilabel(fig);
assert_isequal(class(lbl), 'nelson.ui.control.Label');
assert_isequal(lbl.Type, 'uilabel');
assert_isequal(lbl.Text, 'Label');
assert_isequal(lbl.Position, [100 100 31 22]);
assert_isequal(lbl.HorizontalAlignment, 'left');
assert_isequal(lbl.VerticalAlignment, 'center');
assert_isequal(char(lbl.WordWrap), 'off');
assert_isequal(lbl.Interpreter, 'none');
assert_isequal(lbl.BackgroundColor, 'none');
assert_isequal(lbl.FontName, 'Helvetica');
assert_isequal(lbl.FontSize, 12);
assert_isapprox(lbl.FontColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-12);
assert_isequal(char(lbl.Enable), 'on');
assert_isequal(char(lbl.Visible), 'on');
assert_isequal(lbl.Parent, fig);
%=============================================================================
% set/get
%=============================================================================
lbl.Text = 'Hello';
assert_isequal(lbl.Text, 'Hello');
lbl.Text = {'a'; 'b'; 'c'};
assert_isequal(lbl.Text, {'a'; 'b'; 'c'});
lbl.HorizontalAlignment = 'center';
assert_isequal(lbl.HorizontalAlignment, 'center');
lbl.VerticalAlignment = 'top';
assert_isequal(lbl.VerticalAlignment, 'top');
lbl.WordWrap = 'on';
assert_isequal(char(lbl.WordWrap), 'on');
lbl.BackgroundColor = [1 0 0];
assert_isequal(lbl.BackgroundColor, [1 0 0]);
lbl.Position = [10 10 200 40];
assert_isequal(lbl.Position, [10 10 200 40]);
delete(fig);
%=============================================================================
% no parent: a new uifigure-styled figure is created
%=============================================================================
close all
lbl = uilabel();
fig = lbl.Parent;
assert_isequal(class(fig), 'nelson.ui.Figure');
assert_isequal(fig.MenuBar, 'none');
close all
%=============================================================================
