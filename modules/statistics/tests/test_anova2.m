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
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('anova2'), 3);
asserts.isequal(nargout('anova2'), 3);
%=============================================================================
Y = [8 9 6; 7 8 5; 9 10 7; 12 14 11; 13 15 12; 11 13 10];
[p, tbl, stats] = anova2(Y, 2, 'off');
asserts.isapprox(p, [0.047926866824987 0.002089444381749 0.993662524052472], 1e-12);
asserts.isequal(tbl{1, 1}, 'Source');
asserts.isequal(tbl{2, 1}, 'Columns');
asserts.isequal(tbl{3, 1}, 'Rows');
asserts.isequal(tbl{4, 1}, 'Interaction');
asserts.isequal(tbl{5, 1}, 'Error');
asserts.isapprox(tbl{2, 2}, 27.000000000000000, 1e-12);
asserts.isapprox(tbl{3, 2}, 82.333333333333343, 1e-12);
asserts.isapprox(tbl{4, 2}, 0.666666666666667, 1e-12);
asserts.isapprox(tbl{5, 2}, 28.000000000000000, 1e-12);
asserts.isapprox(tbl{6, 2}, 138.000000000000000, 1e-12);
asserts.isequal([tbl{2, 3} tbl{3, 3} tbl{4, 3} tbl{5, 3} tbl{6, 3}], [2 2 4 9 17]);
asserts.isapprox(tbl{2, 4}, 13.500000000000000, 1e-12);
asserts.isapprox(tbl{3, 4}, 41.166666666666671, 1e-12);
asserts.isapprox(tbl{4, 4}, 0.166666666666667, 1e-12);
asserts.isapprox(tbl{5, 4}, 3.111111111111111, 1e-12);
asserts.isapprox(tbl{2, 5}, 4.339285714285714, 1e-12);
asserts.isapprox(tbl{3, 5}, 13.232142857142858, 1e-12);
asserts.isapprox(tbl{4, 5}, 0.053571428571429, 1e-12);
asserts.isequal(stats.source, 'anova2');
asserts.isapprox(stats.rowmeans, [7.166666666666667 10.500000000000000 12.333333333333334], 1e-12);
asserts.isapprox(stats.colmeans(:)', [10.000000000000000 11.500000000000000 8.500000000000000], 1e-12);
asserts.isapprox(stats.cellmeans, [7.500000000000000 8.500000000000000 5.500000000000000; ...
                                  10.500000000000000 12.000000000000000 9.000000000000000; ...
                                  12.000000000000000 14.000000000000000 11.000000000000000], 1e-12);
asserts.isapprox(stats.sigmasq, 3.111111111111111, 1e-12);
asserts.isequal(stats.df, 9);
%=============================================================================
Z = [8 9 6; 7 8 5; 12 14 11; 13 15 12];
[p2, tbl2, stats2] = anova2(Z, 'off');
asserts.isapprox(p2, [4.555393586003209e-05 6.465256112697659e-07], 1e-12);
asserts.isequal(size(tbl2), [5 6]);
asserts.isequal(tbl2{2, 1}, 'Columns');
asserts.isequal(tbl2{3, 1}, 'Rows');
asserts.isequal(tbl2{4, 1}, 'Error');
asserts.isapprox(tbl2{2, 2}, 18.000000000000000, 1e-12);
asserts.isapprox(tbl2{3, 2}, 99.333333333333343, 1e-12);
asserts.isapprox(tbl2{4, 2}, 0.666666666666657, 1e-12);
asserts.isapprox(tbl2{5, 2}, 118.000000000000000, 1e-12);
asserts.isequal([tbl2{2, 3} tbl2{3, 3} tbl2{4, 3} tbl2{5, 3}], [2 3 6 11]);
asserts.isapprox(tbl2{2, 5}, 81.000000000001151, 1e-10);
asserts.isapprox(tbl2{3, 5}, 298.000000000004263, 1e-10);
asserts.isapprox(stats2.rowmeans, [7.666666666666667 6.666666666666667 ...
                                  12.333333333333334 13.333333333333334], 1e-12);
asserts.isapprox(stats2.colmeans(:)', [10.000000000000000 11.500000000000000 8.500000000000000], 1e-12);
asserts.isequal(stats2.cellmeans, []);
asserts.isapprox(stats2.sigmasq, 0.111111111111110, 1e-12);
%=============================================================================
[ps, tbls, statss] = anova2(single(Y), 2, 'off');
asserts.isequal(class(ps), 'double');
asserts.isapprox(ps, p, 1e-12);
asserts.isapprox(tbls{5, 4}, tbl{5, 4}, 1e-12);
asserts.isapprox(statss.cellmeans, stats.cellmeans, 1e-12);
%=============================================================================
asserts.checkerror('anova2()', _('Not enough input arguments.'));
asserts.checkerror('anova2({1}, 1, ''off'')', sprintf(_('%s must be a real numeric matrix.'), 'Y'));
asserts.checkerror('anova2([1 2 3], 1, ''off'')', sprintf(_('%s must have at least two rows and two columns.'), 'Y'));
asserts.checkerror('anova2([1 2; 3 Inf], 1, ''off'')', _('Y must contain finite observations.'));
asserts.checkerror('anova2([1 2; 3 NaN], 1, ''off'')', _('Y must contain finite observations.'));
asserts.checkerror('anova2([1 2; 3 4], 0, ''off'')', sprintf(_('%s must be a positive integer scalar.'), 'REPS'));
asserts.checkerror('anova2([1 2; 3 4; 5 6], 2, ''off'')', sprintf(_('%s must be a multiple of REPS.'), 'The number of rows in Y'));
asserts.checkerror('anova2([1 2; 3 4], 2, ''off'')', _('Y must contain at least two row-factor levels.'));
asserts.checkerror('anova2([1 2; 3 4], 1, ''bad'')', _('Display option must be ''on'' or ''off''.'));
%=============================================================================
