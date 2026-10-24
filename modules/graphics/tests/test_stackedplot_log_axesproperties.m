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
outages = readtimetable([testsPath, '/fixtures/stackedplot_outage.csv']);
outages = sortrows(outages);
s = stackedplot(outages);
s.AxesProperties(1).YScale = 'log';
s.AxesProperties(2).YScale = 'log';
asserts.isequal(s.Children(1).YScale, 'log');
asserts.isequal(s.Children(2).YScale, 'log');
close all;
%=============================================================================
