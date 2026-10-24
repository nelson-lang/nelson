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
% text edit field
%=============================================================================
fig = uifigure('Visible', 'off');
ef = uieditfield(fig);
assert_isequal(class(ef), 'nelson.ui.control.EditField');
assert_isequal(ef.Type, 'uieditfield');
assert_isequal(ef.Value, '');
assert_isequal(ef.CharacterLimits, [0 Inf]);
assert_isequal(ef.InputType, 'text');
assert_isequal(ef.HorizontalAlignment, 'left');
assert_isequal(char(ef.Editable), 'on');
assert_isequal(ef.Position, [100 100 100 22]);
assert_isequal(ef.BackgroundColor, [1 1 1]);
ef.Value = 'hello';
assert_isequal(ef.Value, 'hello');
%=============================================================================
% numeric edit field
%=============================================================================
nef = uieditfield(fig, 'numeric');
assert_isequal(class(nef), 'nelson.ui.control.NumericEditField');
assert_isequal(nef.Type, 'uinumericeditfield');
assert_isequal(nef.Value, 0);
assert_isequal(nef.Limits, [-Inf Inf]);
assert_isequal(char(nef.LowerLimitInclusive), 'on');
assert_isequal(char(nef.UpperLimitInclusive), 'on');
assert_isequal(char(nef.RoundFractionalValues), 'off');
assert_isequal(nef.ValueDisplayFormat, '%11.4g');
assert_isequal(char(nef.AllowEmpty), 'off');
assert_isequal(nef.HorizontalAlignment, 'right');
nef.Value = 42.5;
assert_isequal(nef.Value, 42.5);
nef.Value = -42.5;
assert_isequal(nef.Value, -42.5);
nef.Limits = [0 100];
assert_isequal(nef.Limits, [0 100]);
delete(fig);
close all
%=============================================================================
