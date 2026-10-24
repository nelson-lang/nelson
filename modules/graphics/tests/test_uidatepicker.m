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
% defaults
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 420 260]);
dp = uidatepicker(fig);
assert_isequal(class(dp), 'nelson.ui.control.DatePicker');
assert_isequal(dp.Type, 'uidatepicker');
assert_isequal(char(dp.DisplayFormat), 'dd-MMM-uuuu');
assert_isequal(char(dp.Editable), 'on');
assert_istrue(isnat(dp.Value));
%=============================================================================
% value round-trip
%=============================================================================
dp.Value = datetime(2026, 7, 19);
assert_isequal(dp.Value, datetime(2026, 7, 19));
%=============================================================================
% DisplayFormat drives the rendered text: the default (dd-MMM-uuuu) and an
% explicit ISO format must not produce the same export. A renderer that
% hardcodes the ISO date would make both identical.
%=============================================================================
dp.Position = [120 115 180 24];
dp.DisplayFormat = 'dd-MMM-uuuu';
drawnow();
defaultPng = [tempname(), '.png'];
exportapp(fig, defaultPng);
assert_istrue(isfile(defaultPng));
dp.DisplayFormat = 'yyyy-MM-dd';
assert_isequal(char(dp.DisplayFormat), 'yyyy-MM-dd');
drawnow();
isoPng = [tempname(), '.png'];
exportapp(fig, isoPng);
assert_istrue(isfile(isoPng));
assert_isfalse(isequal(imread(defaultPng), imread(isoPng)));
rmfile(defaultPng);
rmfile(isoPng);
delete(fig);
close all
%=============================================================================
