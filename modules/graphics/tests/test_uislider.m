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
sld = uislider(fig);
assert_isequal(class(sld), 'nelson.ui.control.Slider');
assert_isequal(sld.Type, 'uislider');
assert_isequal(sld.Value, 0);
assert_isequal(sld.Limits, [0 100]);
assert_isequal(sld.Orientation, 'horizontal');
assert_isequal(sld.Position, [100 100 150 3]);
assert_isequal(sld.MajorTicks, [0 20 40 60 80 100]);
assert_isequal(sld.MajorTicksMode, 'auto');
assert_isequal(sld.MinorTicksMode, 'auto');
sld.Value = 42;
assert_isequal(sld.Value, 42);
sld.Limits = [-100 100];
sld.Value = -42;
assert_isequal(sld.Value, -42);
sld.Value = 0;
sld.Limits = [0 10];
assert_isequal(sld.Limits, [0 10]);
assert_isequal(sld.MajorTicks, [0 2 4 6 8 10]);
sld.MajorTicks = [0 5 10];
assert_isequal(sld.MajorTicks, [0 5 10]);
%=============================================================================
rs = uislider(fig, 'range');
assert_isequal(class(rs), 'nelson.ui.control.RangeSlider');
assert_isequal(rs.Type, 'uirangeslider');
assert_isequal(rs.Value, [0 100]);
rs.Value = [20 60];
assert_isequal(rs.Value, [20 60]);
rs.Limits = [-100 100];
rs.Value = [-60 20];
assert_isequal(rs.Value, [-60 20]);
delete(fig);
close all
%=============================================================================
