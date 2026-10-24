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
% uipanel: defaults
%=============================================================================
fig = uifigure('Visible', 'off');
p = uipanel(fig);
assert_isequal(class(p), 'nelson.ui.container.Panel');
assert_isequal(p.Type, 'uipanel');
assert_isequal(p.Title, '');
assert_isequal(p.TitlePosition, 'lefttop');
assert_isequal(p.Position, [20 20 260 221]);
assert_isequal(p.BorderType, 'line');
assert_isequal(p.BorderWidth, 1);
assert_isapprox(p.BorderColor, [0.490196078431373 0.490196078431373 0.490196078431373], 1e-12);
assert_isapprox(p.ForegroundColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-12);
assert_isapprox(p.BackgroundColor, [0.96078431372549 0.96078431372549 0.96078431372549], 1e-12);
assert_isequal(p.FontUnits, 'points');
assert_isequal(p.FontSize, 8);
assert_isequal(p.Units, 'pixels');
assert_isequal(char(p.AutoResizeChildren), 'off');
assert_isequal(char(p.Scrollable), 'off');
assert_isequal(char(p.Visible), 'on');
assert_isequal(char(p.Enable), 'on');
assert_isequal(p.Parent, fig);
assert_istrue(isempty(p.Children));
%=============================================================================
% components nested inside a panel
%=============================================================================
btn = uibutton(p, 'Text', 'inside');
assert_isequal(btn.Parent, p);
assert_isequal(numel(p.Children), 1);
assert_isequal(p.Children(1), btn);
lbl = uilabel(p);
assert_isequal(numel(p.Children), 2);
%=============================================================================
% nested panels
%=============================================================================
p2 = uipanel(p, 'Title', 'Nested');
assert_isequal(p2.Parent, p);
assert_isequal(p2.Title, 'Nested');
b2 = uibutton(p2);
assert_isequal(b2.Parent, p2);
%=============================================================================
% set/get
%=============================================================================
p.Title = 'My Panel';
assert_isequal(p.Title, 'My Panel');
p.TitlePosition = 'centertop';
assert_isequal(p.TitlePosition, 'centertop');
p.BorderType = 'none';
assert_isequal(p.BorderType, 'none');
p.Position = [10 10 300 200];
assert_isequal(p.Position, [10 10 300 200]);
delete(fig);
%=============================================================================
% nested panels: hidden siblings must not be painted by getframe
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 420 560], 'Color', [1 1 1]);
parent = uipanel(fig, 'Visible', 'off', 'Position', [20 20 370 526], ...
  'BorderType', 'none');
child1 = uipanel(parent, 'Visible', 'off', 'Position', [0 0 370 526], ...
  'BorderType', 'none');
child2 = uipanel(parent, 'Visible', 'off', 'Position', [0 0 370 526], ...
  'BorderType', 'none');
child3 = uipanel(parent, 'Visible', 'off', 'Position', [0 0 370 526], ...
  'BorderType', 'none');
uibutton(child1, 'Text', 'red', 'Position', [40 420 120 40], ...
  'BackgroundColor', [1 0 0]);
uibutton(child2, 'Text', 'green', 'Position', [40 420 120 40], ...
  'BackgroundColor', [0 1 0]);
uibutton(child3, 'Text', 'blue', 'Position', [40 420 120 40], ...
  'BackgroundColor', [0 0 1]);
parent.Visible = 'on';
child2.Visible = 'on';
assert_isequal(char(child1.Visible), 'off');
assert_isequal(char(child2.Visible), 'on');
assert_isequal(char(child3.Visible), 'off');
drawnow();
frame = getframe(fig);
image = frame.cdata;
R = double(image(:, :, 1));
G = double(image(:, :, 2));
B = double(image(:, :, 3));
redPixels = nnz((R > 220) & (G < 60) & (B < 60));
greenPixels = nnz((R < 60) & (G > 220) & (B < 60));
bluePixels = nnz((R < 60) & (G < 60) & (B > 220));
asserts.istrue(greenPixels > 1000);
assert_isequal(redPixels, 0);
assert_isequal(bluePixels, 0);

savedImageName = [tempname(), '.png'];
saveas(fig, savedImageName);
assert_istrue(isfile(savedImageName));
savedImage = imread(savedImageName);
savedR = double(savedImage(:, :, 1));
savedG = double(savedImage(:, :, 2));
savedB = double(savedImage(:, :, 3));
savedRedPixels = nnz((savedR > 220) & (savedG < 60) & (savedB < 60));
savedGreenPixels = nnz((savedR < 60) & (savedG > 220) & (savedB < 60));
savedBluePixels = nnz((savedR < 60) & (savedG < 60) & (savedB > 220));
assert_istrue(savedGreenPixels > 1000);
assert_isequal(savedRedPixels, 0);
assert_isequal(savedBluePixels, 0);
rmfile(savedImageName);
delete(fig);
%=============================================================================
% no parent: a new uifigure-styled figure is created
%=============================================================================
close all
p = uipanel();
assert_isequal(class(p.Parent), 'nelson.ui.Figure');
close all
%=============================================================================
