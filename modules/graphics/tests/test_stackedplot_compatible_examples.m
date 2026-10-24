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
s = stackedplot(outages);
asserts.istrue(isgraphics(s, 'StackedLineChart'));
asserts.isequal(s.DisplayVariables, {'Loss', 'Customers', 'RestorationTime'});
asserts.isequal(s.DisplayLabels, {'Loss'; 'Customers'; 'RestorationTime'});
asserts.isequal(length(s.Children), 3);
asserts.isequal(s.XLabel, 'OutageTime');
asserts.isequal(s.Children(1).Children(end).YData, outages.Loss');
asserts.isequal(s.Children(3).Children(end).YData, datenum(outages.RestorationTime)');
close all;
%=============================================================================
