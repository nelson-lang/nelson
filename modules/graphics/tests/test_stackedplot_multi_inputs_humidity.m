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
skip_if_portable_graphics_backend('stackedplot text table import is not stable yet.');
testsPath = [modulepath('graphics'), '/tests'];
indoors = readtimetable([testsPath, '/fixtures/stackedplot_inside.csv']);
outdoors = readtimetable([testsPath, '/fixtures/stackedplot_weather.csv']);
s = stackedplot(indoors, outdoors, "Humidity");
asserts.isequal(length(s.Children), 1);
asserts.isequal(s.DisplayVariables, {'Humidity'});
asserts.isequal(s.DisplayLabels, {'Humidity'});
asserts.isequal(length(graphics_stackedplot_line_handles(s.Children(1))), 2);
close all;
%=============================================================================
