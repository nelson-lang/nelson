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
cbx = uicheckbox(fig);
assert_isequal(class(cbx), 'nelson.ui.control.CheckBox');
assert_isequal(cbx.Type, 'uicheckbox');
assert_isequal(cbx.Value, false);
assert_isequal(cbx.Text, 'Check Box');
assert_isequal(cbx.Position, [100 100 84 22]);
assert_isequal(char(cbx.WordWrap), 'off');
assert_isequal(cbx.FontName, 'Helvetica');
assert_isequal(cbx.FontSize, 12);
assert_isequal(char(cbx.Enable), 'on');
%=============================================================================
cbx.Value = true;
assert_isequal(cbx.Value, true);
cbx.Value = 0;
assert_isequal(cbx.Value, false);
cbx.Text = 'Accept';
assert_isequal(cbx.Text, 'Accept');
cbx.ValueChangedFcn = @(s, e) disp(e.Value);
assert_istrue(isa(cbx.ValueChangedFcn, 'function_handle'));
delete(fig);
close all
%=============================================================================
