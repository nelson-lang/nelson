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
outages = readtimetable([testsPath, '/fixtures/stackedplot_outage.csv']);
outages = sortrows(outages);
outdoors = readtimetable([testsPath, '/fixtures/stackedplot_weather.csv']);
s = stackedplot(outdoors);
s.LineWidth = 2;
s.LineProperties(2).PlotType = 'scatter';
s.LineProperties(3).PlotType = 'stairs';
s.AxesProperties(1).YLimits = [48 50];
asserts.isequal(s.Children(1).Children(end).LineWidth, 2);
asserts.isequal(s.LineProperties(2).PlotType, 'scatter');
asserts.isequal(s.Children(2).Children(end).LineStyle, 'none');
asserts.isequal(s.Children(2).Children(end).Marker, 'o');
asserts.isequal(s.LineProperties(3).PlotType, 'stairs');
asserts.istrue(length(s.Children(3).Children(end).XData) > height(outdoors));
asserts.isequal(s.Children(1).YLim, [48 50]);
close all;
%=============================================================================
