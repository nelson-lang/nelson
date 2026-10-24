%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('stackedplot text table import is not stable yet.');
testsPath = [modulepath('graphics'), '/tests'];
%=============================================================================
indoors = readtimetable([testsPath, '/fixtures/stackedplot_inside.csv']);
outdoors = readtimetable([testsPath, '/fixtures/stackedplot_weather.csv']);
s = stackedplot(indoors, outdoors);
asserts.isequal(s.DisplayVariables, {'Humidity', 'AirQuality', 'TemperatureF', 'PressureHg'});
asserts.isequal(s.DisplayLabels, {'Humidity'; 'AirQuality'; 'TemperatureF'; 'PressureHg'});
asserts.isequal(length(s.Children), 4);
asserts.isequal(length(graphics_stackedplot_line_handles(s.Children(1))), 2);
asserts.isequal(s.AxesProperties(1).LegendLabels, {'Input 1', 'Input 2'});
expectedColors = s.Children(1).ColorOrder(1:2, :);
lines = graphics_stackedplot_line_handles(s.Children(1));
asserts.isequal(lines(1).Color, expectedColors(1, :));
asserts.isequal(lines(2).Color, expectedColors(2, :));
close all;
%=============================================================================
