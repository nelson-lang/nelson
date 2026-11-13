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
% a figure with uifigure-like properties is still a figure: containers fill it
f = figure('Visible', 'off', 'MenuBar', 'none', 'ToolBar', 'none', ...
  'NumberTitle', 'off', 'IntegerHandle', 'off');
p = uipanel(f);
assert_isequal(p.Units, 'normalized');
assert_isequal(p.Position, [0 0 1 1]);
bg = uibuttongroup(f);
assert_isequal(bg.Units, 'normalized');
assert_isequal(bg.Position, [0 0 1 1]);
tg = uitabgroup(f);
assert_isequal(tg.Units, 'normalized');
assert_isequal(tg.Position, [0 0 1 1]);
delete(f);
%=============================================================================
% dialog is a figure too
d = dialog('Visible', 'off');
p = uipanel(d);
assert_isequal(p.Units, 'normalized');
assert_isequal(p.Position, [0 0 1 1]);
delete(d);
%=============================================================================
% uifigure keeps pixels
u = uifigure('Visible', 'off');
p = uipanel(u);
assert_isequal(p.Units, 'pixels');
assert_isequal(p.Position, [20 20 260 221]);
bg = uibuttongroup(u);
assert_isequal(bg.Units, 'pixels');
tg = uitabgroup(u);
assert_isequal(tg.Units, 'pixels');
delete(u);
%=============================================================================
% implicit uifigure of a component without parent keeps pixels
b = uibutton('Visible', 'off');
u = b.Parent;
p = uipanel(u);
assert_isequal(p.Units, 'pixels');
assert_isequal(p.Position, [20 20 260 221]);
delete(u);
%=============================================================================
% uiaxes layout follows the uiaxes, not the figure
f = figure('Visible', 'off');
a = uiaxes(f);
assert_isapprox(a.InnerPosition, [30.1333 26.9333 374.8667 276.2667], 1e-2);
x = axes(f, 'Units', 'pixels');
x.InnerPosition = [50 50 300 200];
assert_isequal(x.Position, [50 50 300 200]);
delete(f);
u = uifigure('Visible', 'off');
x = axes(u, 'Units', 'pixels');
x.InnerPosition = [50 50 300 200];
assert_isequal(x.Position, [50 50 300 200]);
x.Position = [20 20 300 200];
assert_isequal(x.InnerPosition, [20 20 300 200]);
delete(u);
%=============================================================================
% pixel font units mean pixels in a figure too (20 px = 15 pt)
f = figure('Visible', 'off');
x1 = axes(f, 'FontUnits', 'pixels', 'FontSize', 20);
x2 = axes(f, 'FontUnits', 'points', 'FontSize', 15);
assert_isapprox(x1.TightInset, x2.TightInset, 1e-6);
delete(f);
%=============================================================================
