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
patients = readtable([testsPath, '/fixtures/stackedplot_patients.csv']);
s = stackedplot(patients, {{'Systolic', 'Diastolic'}, 'Weight'});
asserts.isequal(s.DisplayLabels, {'Systolic, Diastolic'; 'Weight'});
asserts.isequal(s.AxesProperties(1).LegendLabels, {'Systolic', 'Diastolic'});
asserts.isequal(length(graphics_stackedplot_line_handles(s.Children(1))), 2);
s.LineProperties(1).Color = [1 0 1; 0 0.5 0];
s.LineProperties(1).LineStyle = {'--', ':'};
lines = graphics_stackedplot_line_handles(s.Children(1));
asserts.isequal(lines(1).Color, [1 0 1]);
asserts.isequal(lines(2).Color, [0 0.5 0]);
asserts.isequal(lines(1).LineStyle, '--');
asserts.isequal(lines(2).LineStyle, ':');
close all;
%=============================================================================
