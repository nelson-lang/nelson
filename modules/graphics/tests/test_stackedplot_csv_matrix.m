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
matrixTable = readtable([testsPath, '/fixtures/stackedplot_matrix.csv']);
X = matrixTable.X;
Y = [matrixTable.A, matrixTable.B, matrixTable.C];
s = stackedplot(X, Y);
asserts.isequal(s.DisplayVariables, {});
asserts.isequal(s.DisplayLabels, {'Column 1'; 'Column 2'; 'Column 3'});
asserts.isequal(s.Children(1).Children(end).XData, X');
asserts.isequal(s.Children(2).Children(end).YData, matrixTable.B');
close all;
%=============================================================================
