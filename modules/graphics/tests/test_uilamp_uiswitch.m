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
lmp = uilamp(fig);
assert_isequal(class(lmp), 'nelson.ui.control.Lamp');
assert_isequal(lmp.Type, 'uilamp');
assert_isequal(lmp.Color, [0 1 0]);
assert_isequal(lmp.Position, [100 100 20 20]);
lmp.Color = 'red';
assert_isequal(lmp.Color, [1 0 0]);
%=============================================================================
sw = uiswitch(fig);
assert_isequal(class(sw), 'nelson.ui.control.Switch');
assert_isequal(sw.Type, 'uiswitch');
assert_isequal(sw.Items, {'Off', 'On'});
assert_isequal(sw.Value, 'Off');
assert_isequal(sw.ValueIndex, 1);
assert_isequal(sw.Orientation, 'horizontal');
assert_isequal(sw.Position, [100 100 45 20]);
sw.Value = 'On';
assert_isequal(sw.Value, 'On');
assert_isequal(sw.ValueIndex, 2);
%=============================================================================
rsw = uiswitch(fig, 'rocker');
assert_isequal(class(rsw), 'nelson.ui.control.RockerSwitch');
assert_isequal(rsw.Type, 'uirockerswitch');
assert_isequal(rsw.Orientation, 'vertical');
assert_isequal(rsw.Position, [100 100 20 45]);
%=============================================================================
tsw = uiswitch(fig, 'toggle');
assert_isequal(class(tsw), 'nelson.ui.control.ToggleSwitch');
assert_isequal(tsw.Type, 'uitoggleswitch');
%=============================================================================
% custom items
sw2 = uiswitch(fig, 'Items', {'Stop', 'Go'});
assert_isequal(sw2.Value, 'Stop');
sw2.Value = 'Go';
assert_isequal(sw2.ValueIndex, 2);
sw2.ItemsData = [-1 1];
sw2.Value = -1;
assert_isequal(sw2.Value, -1);
assert_isequal(sw2.ValueIndex, 1);
%=============================================================================
% datepicker
dp = uidatepicker(fig);
assert_isequal(class(dp), 'nelson.ui.control.DatePicker');
assert_isequal(dp.Type, 'uidatepicker');
assert_isequal(dp.DisplayFormat, 'dd-MMM-uuuu');
assert_istrue(isdatetime(dp.Value));
d = datetime(2026, 7, 18);
dp.Value = d;
assert_isequal(dp.Value, d);
delete(fig);
close all
%=============================================================================
