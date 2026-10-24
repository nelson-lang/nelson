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
asserts.isequal(nargin('anova1'), 3);
asserts.isequal(nargout('anova1'), 3);
%=============================================================================
X = [6 7 8; 5 7 9; 4 8 NaN; 6 9 10];
[p, tbl, stats] = anova1(X, [], 'off');
asserts.isapprox(p, 0.002471774786825, 1e-12);
asserts.isequal(tbl{1, 1}, 'Source');
asserts.isequal(tbl{1, 6}, 'Prob>F');
asserts.isapprox(tbl{2, 2}, 26.136363636363640, 1e-12);
asserts.isapprox(tbl{3, 2}, 7.500000000000000, 1e-12);
asserts.isapprox(tbl{4, 2}, 33.636363636363626, 1e-12);
asserts.isequal(tbl{2, 3}, 2);
asserts.isequal(tbl{3, 3}, 8);
asserts.isequal(tbl{4, 3}, 10);
asserts.isapprox(tbl{2, 4}, 13.068181818181820, 1e-12);
asserts.isapprox(tbl{3, 4}, 0.937500000000000, 1e-12);
asserts.isapprox(tbl{2, 5}, 13.939393939393941, 1e-12);
asserts.isapprox(tbl{2, 6}, p, 1e-12);
asserts.isequal(tbl{3, 5}, []);
asserts.isequal(tbl{4, 6}, []);
asserts.isequal(stats.gnames, {'1'; '2'; '3'});
asserts.isequal(stats.n, [4 4 3]);
asserts.isapprox(stats.means, [5.250000000000000 7.750000000000000 9.000000000000000], 1e-12);
asserts.isequal(stats.df, 8);
asserts.isapprox(stats.s, 0.968245836551854, 1e-12);
asserts.isequal(stats.source, 'anova1');
%=============================================================================
y = [1 2 3 4 NaN 1.5 2.5 3.5 5 6]';
g = {'b', 'b', 'a', 'a', 'a', 'b', 'b', 'a', 'c', 'c'}';
[p2, tbl2, stats2] = anova1(y, g, 'off');
asserts.isapprox(p2, 0.001146125498384, 1e-12);
asserts.isapprox(tbl2{2, 2}, 19.250000000000000, 1e-12);
asserts.isapprox(tbl2{3, 2}, 2.250000000000000, 1e-12);
asserts.isapprox(tbl2{4, 2}, 21.500000000000000, 1e-12);
asserts.isapprox(tbl2{2, 4}, 9.625000000000000, 1e-12);
asserts.isapprox(tbl2{3, 4}, 0.375000000000000, 1e-12);
asserts.isapprox(tbl2{2, 5}, 25.666666666666668, 1e-12);
asserts.isequal(stats2.gnames, {'a'; 'b'; 'c'});
asserts.isequal(stats2.n, [3 4 2]);
asserts.isapprox(stats2.means, [3.500000000000000 1.750000000000000 5.500000000000000], 1e-12);
%=============================================================================
[p3, tbl3, stats3] = anova1([1 2; 3 NaN; 5 7], {'left', 'right'}, 'off');
asserts.isapprox(p3, 0.574220426452033, 1e-12);
asserts.isequal(stats3.gnames, {'left'; 'right'});
asserts.isequal(stats3.n, [3 2]);
asserts.isapprox(stats3.means, [3.000000000000000 4.500000000000000], 1e-12);
asserts.isapprox(tbl3{2, 5}, 0.395121951219512, 1e-12);
[p3b, ~, stats3b] = anova1([1 2; 3 NaN; 5 7], ['left '; 'right'], 'off');
asserts.isapprox(p3b, p3, 1e-12);
asserts.isequal(stats3b.gnames, {'left'; 'right'});
%=============================================================================
[p4, tbl4, stats4] = anova1(single([1 2 3; 2 4 6; 3 6 9]), [], 'off');
asserts.isequal(class(p4), 'double');
asserts.isapprox(p4, 0.156121984524351, 1e-12);
asserts.isapprox(stats4.s, 2.160246899469287, 1e-12);
asserts.isequal(tbl4{2, 3}, 2);
asserts.isequal(tbl4{3, 3}, 6);
%=============================================================================
[p5, tbl5, stats5] = anova1([1 2; 3 4; 5 7], 'off');
asserts.isapprox(p5, 0.512225258699542, 1e-12);
asserts.isapprox(tbl5{2, 2}, 2.666666666666666, 1e-12);
asserts.isapprox(stats5.means, [3.000000000000000 4.333333333333333], 1e-12);
%=============================================================================
asserts.checkerror('anova1()', _('Not enough input arguments.'));
asserts.checkerror('anova1({1}, [], ''off'')', sprintf(_('%s must be a real numeric vector or matrix.'), 'X'));
asserts.checkerror('anova1([1 2 3], [1 2], ''off'')', _('GROUP must have the expected number of labels.'));
asserts.checkerror('anova1([1 2; 3 4], {''only''}, ''off'')', _('GROUP must have the expected number of labels.'));
asserts.checkerror('anova1([1 2; 3 4], {1}, ''off'')', sprintf(_('%s must be a vector or text array.'), 'GROUP'));
asserts.checkerror('anova1([1 2; 3 4], [], ''bad'')', _('Display option must be ''on'' or ''off''.'));
asserts.checkerror('anova1([1 NaN], [1 2], ''off'')', _('At least two groups with more observations than groups are required.'));
%=============================================================================
