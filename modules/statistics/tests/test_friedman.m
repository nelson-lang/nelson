%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('friedman'), 3);
asserts.isequal(nargout('friedman'), 3);
%=============================================================================
X = [9 7 6; 8 6 5; 7 8 6; 10 9 7; 9 10 8];
[p, tbl, stats] = friedman(X, 'off');
asserts.isapprox(p, 0.022370771856166, 1e-12);
asserts.isequal(tbl{1, 1}, 'Source');
asserts.isequal(tbl{1, 5}, 'Prob>Chi-sq');
asserts.isequal(tbl{2, 1}, 'Columns');
asserts.isapprox(tbl{2, 2}, 7.599999999999994, 1e-12);
asserts.isapprox(tbl{3, 2}, 2.400000000000006, 1e-12);
asserts.isapprox(tbl{4, 2}, 10.000000000000000, 1e-12);
asserts.isequal([tbl{2, 3} tbl{3, 3} tbl{4, 3}], [2 8 10]);
asserts.isapprox(tbl{2, 4}, 7.600000000000009, 1e-12);
asserts.isapprox(tbl{2, 5}, p, 1e-12);
asserts.isequal(stats.source, 'friedman');
asserts.isequal(stats.n, 5);
asserts.isequal(stats.reps, 1);
asserts.isequal(stats.df, 2);
asserts.isapprox(stats.meanranks, [2.600000000000000 2.400000000000000 1.000000000000000], 1e-12);
asserts.isequal(stats.sumranks, [13 12 5]);
%=============================================================================
Y = [9 7 6; 8 6 5; 7 8 6; 6 7 5; 10 9 7; 9 10 8];
[p2, tbl2, stats2] = friedman(Y, 2, 'off');
asserts.isapprox(p2, 0.011108996538242, 1e-12);
asserts.isapprox(tbl2{2, 2}, 9.000000000000000, 1e-12);
asserts.isapprox(tbl2{3, 2}, 3.000000000000000, 1e-12);
asserts.isapprox(tbl2{4, 2}, 12.000000000000000, 1e-12);
asserts.isequal([tbl2{2, 3} tbl2{3, 3} tbl2{4, 3}], [2 10 12]);
asserts.isapprox(tbl2{2, 4}, 9.000000000000000, 1e-12);
asserts.isapprox(tbl2{2, 5}, p2, 1e-12);
asserts.isequal(stats2.n, 3);
asserts.isequal(stats2.reps, 2);
asserts.isapprox(stats2.meanranks, [2.500000000000000 2.500000000000000 1.000000000000000], 1e-12);
asserts.isequal(stats2.sumranks, [15 15 6]);
%=============================================================================
[p3, tbl3, stats3] = friedman([1 1 1; 2 2 2; 3 3 3], 'off');
asserts.isequal(p3, 1);
asserts.isequal(tbl3{2, 2}, 0);
asserts.isequal(tbl3{2, 4}, 0);
asserts.isequal(stats3.meanranks, [2 2 2]);
%=============================================================================
[ps, ~, statss] = friedman(single(X), 'off');
asserts.isequal(class(ps), 'double');
asserts.isapprox(ps, p, 1e-12);
asserts.isapprox(statss.meanranks, stats.meanranks, 1e-12);
%=============================================================================
asserts.checkerror('friedman()', _('Not enough input arguments.'));
asserts.checkerror('friedman({1}, 1, ''off'')', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('friedman([1 2 3], 1, ''off'')', sprintf(_('%s must have at least two rows and two columns.'), 'X'));
asserts.checkerror('friedman([1 2; 3 NaN], 1, ''off'')', _('X must contain non-NaN observations.'));
asserts.checkerror('friedman([1 2; 3 4], 0, ''off'')', sprintf(_('%s must be a positive integer scalar.'), 'REPS'));
asserts.checkerror('friedman([1 2; 3 4; 5 6], 2, ''off'')', sprintf(_('%s must be a multiple of REPS.'), 'The number of rows in X'));
asserts.checkerror('friedman([1 2; 3 4], 2, ''off'')', _('X must contain at least two block levels.'));
asserts.checkerror('friedman([1 2; 3 4], 1, ''bad'')', _('Display option must be ''on'' or ''off''.'));
%=============================================================================
