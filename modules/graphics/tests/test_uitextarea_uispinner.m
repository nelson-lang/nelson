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
% uitextarea
%=============================================================================
fig = uifigure('Visible', 'off');
ta = uitextarea(fig);
assert_isequal(class(ta), 'nelson.ui.control.TextArea');
assert_isequal(ta.Type, 'uitextarea');
assert_isequal(ta.Position, [100 100 150 60]);
assert_isequal(char(ta.WordWrap), 'on');
assert_isequal(char(ta.Editable), 'on');
assert_isequal(ta.HorizontalAlignment, 'left');
assert_isequal(ta.BackgroundColor, [1 1 1]);
ta.Value = {'line one'; 'line two'};
assert_isequal(ta.Value, {'line one'; 'line two'});
ta.Value = 'single';
assert_isequal(ta.Value, {'single'});
%=============================================================================
% uispinner
%=============================================================================
sp = uispinner(fig);
assert_isequal(class(sp), 'nelson.ui.control.Spinner');
assert_isequal(sp.Type, 'uispinner');
assert_isequal(sp.Value, 0);
assert_isequal(sp.Step, 1);
assert_isequal(sp.Limits, [-Inf Inf]);
assert_isequal(sp.ValueDisplayFormat, '%11.4g');
assert_isequal(sp.HorizontalAlignment, 'right');
assert_isequal(sp.Position, [100 100 100 22]);
sp.Value = 5;
assert_isequal(sp.Value, 5);
sp.Value = -5;
assert_isequal(sp.Value, -5);
sp.Step = 0.5;
assert_isequal(sp.Step, 0.5);
sp.Limits = [0 10];
assert_isequal(sp.Limits, [0 10]);
delete(fig);
close all
%=============================================================================
