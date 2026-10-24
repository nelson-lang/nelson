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
outageTable = readtable([testsPath, '/fixtures/stackedplot_outage.csv']);
outageTable = sortrows(outageTable);
s = stackedplot(outageTable, {'Loss', 'Customers'});
asserts.isequal(s.DisplayVariables, {'Loss', 'Customers'});
asserts.isequal(s.DisplayLabels, {'Loss'; 'Customers'});
asserts.isequal(s.XLabel, 'Row');
asserts.isequal(s.Children(1).Children(end).YData, outageTable.Loss');
close all;
%=============================================================================
