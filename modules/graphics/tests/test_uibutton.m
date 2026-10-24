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
skip_if_portable_graphics_backend('native button event state is not available.');
% uibutton: push button (default style)
%=============================================================================
fig = uifigure('Visible', 'off');
btn = uibutton(fig);
assert_isequal(class(btn), 'nelson.ui.control.Button');
assert_isequal(btn.Type, 'uibutton');
assert_isequal(btn.Text, 'Button');
assert_isequal(btn.Position, [100 100 100 22]);
assert_isequal(btn.InnerPosition, [100 100 100 22]);
assert_isequal(btn.OuterPosition, [100 100 100 22]);
assert_isequal(btn.HorizontalAlignment, 'center');
assert_isequal(btn.VerticalAlignment, 'center');
assert_isequal(btn.IconAlignment, 'left');
assert_isequal(btn.Icon, '');
assert_isequal(char(btn.WordWrap), 'off');
assert_isequal(btn.Interpreter, 'none');
assert_isequal(char(btn.Enable), 'on');
assert_isequal(char(btn.Visible), 'on');
assert_isequal(btn.Tooltip, '');
assert_isequal(btn.FontName, 'Helvetica');
assert_isequal(btn.FontSize, 12);
assert_isequal(btn.FontWeight, 'normal');
assert_isequal(btn.FontAngle, 'normal');
assert_isapprox(btn.FontColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-12);
assert_isapprox(btn.BackgroundColor, [0.96078431372549 0.96078431372549 0.96078431372549], 1e-12);
assert_isequal(char(btn.Interruptible), 'on');
assert_isequal(btn.BusyAction, 'queue');
assert_isequal(char(btn.BeingDeleted), 'off');
assert_isequal(char(btn.HandleVisibility), 'on');
assert_isequal(btn.Tag, '');
assert_isequal(btn.UserData, []);
assert_isequal(btn.Parent, fig);
%=============================================================================
% set/get on main properties
%=============================================================================
btn.Text = 'Click me';
assert_isequal(btn.Text, 'Click me');
btn.Text = {'line 1'; 'line 2'};
assert_isequal(btn.Text, {'line 1'; 'line 2'});
btn.Position = [10 20 120 30];
assert_isequal(btn.Position, [10 20 120 30]);
assert_isequal(btn.InnerPosition, [10 20 120 30]);
btn.Enable = 'off';
assert_isequal(char(btn.Enable), 'off');
btn.FontWeight = 'bold';
assert_isequal(btn.FontWeight, 'bold');
btn.HorizontalAlignment = 'left';
assert_isequal(btn.HorizontalAlignment, 'left');
% invalid enum value must error
didError = false;
try
  btn.HorizontalAlignment = 'top';
catch
  didError = true;
end
assert_istrue(didError);
assert_isequal(btn.HorizontalAlignment, 'left');
%=============================================================================
% callback property
%=============================================================================
btn.ButtonPushedFcn = @(src, event) disp('pushed');
assert_istrue(isa(btn.ButtonPushedFcn, 'function_handle'));
%=============================================================================
delete(fig);
%=============================================================================
% A true-color icon fills an image-only button while preserving its aspect ratio.
fig = uifigure('Visible', 'on', 'Position', [100 100 220 140]);
icon = zeros(60, 120, 3);
icon(:, :, 2) = 1;
uibutton(fig, 'Text', '', 'Icon', icon, 'Position', [30 30 160 80]);
drawnow();
frame = getframe(fig);
pixels = double(frame.cdata);
greenPixels = pixels(:, :, 1) < 60 & pixels(:, :, 2) > 200 & pixels(:, :, 3) < 60;
assert_istrue(nnz(greenPixels) > 1000);
delete(fig);
%=============================================================================
% uibutton: state style
%=============================================================================
fig = uifigure('Visible', 'off');
sbtn = uibutton(fig, 'state');
assert_isequal(class(sbtn), 'nelson.ui.control.StateButton');
assert_isequal(sbtn.Type, 'uistatebutton');
assert_isequal(sbtn.Text, 'State Button');
assert_isequal(sbtn.Value, false);
sbtn.Value = true;
assert_isequal(sbtn.Value, true);
sbtn.Value = 0;
assert_isequal(sbtn.Value, false);
delete(fig);
%=============================================================================
% no parent: a new uifigure-styled figure is created
%=============================================================================
close all
btn = uibutton();
fig = btn.Parent;
assert_isequal(class(fig), 'nelson.ui.Figure');
assert_isequal(fig.MenuBar, 'none');
assert_isequal(fig.ToolBar, 'none');
assert_isequal(char(fig.NumberTitle), 'off');
close all
%=============================================================================
% name/value at creation
%=============================================================================
fig = uifigure('Visible', 'off');
btn = uibutton(fig, 'Text', 'OK', 'Position', [5 5 60 25], 'Tag', 'okbtn');
assert_isequal(btn.Text, 'OK');
assert_isequal(btn.Position, [5 5 60 25]);
assert_isequal(btn.Tag, 'okbtn');
delete(fig);
%=============================================================================
