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
asserts.isequal(nargin('kruskalwallis'), 3);
asserts.isequal(nargout('kruskalwallis'), 3);
%=============================================================================
X = [6 7 8; 5 7 9; 4 8 NaN; 6 9 10];
[p, tbl, stats] = kruskalwallis(X, [], 'off');
asserts.isapprox(p, 0.018372255965478, 1e-12);
asserts.isequal(tbl{1, 1}, 'Source');
asserts.isequal(tbl{1, 5}, 'Prob>Chi-sq');
asserts.isapprox(tbl{2, 2}, 86.333333333333314, 1e-12);
asserts.isapprox(tbl{3, 2}, 23.666666666666686, 1e-12);
asserts.isapprox(tbl{4, 2}, 110.000000000000000, 1e-12);
asserts.isequal([tbl{2, 3} tbl{3, 3} tbl{4, 3}], [2 8 10]);
asserts.isapprox(tbl{2, 4}, 7.993827160493830, 1e-12);
asserts.isapprox(tbl{2, 5}, p, 1e-12);
asserts.isequal(stats.gnames, {'1'; '2'; '3'});
asserts.isequal(stats.n, [4 4 3]);
asserts.isapprox(stats.meanranks, [2.500000000000000 7.000000000000000 9.333333333333334], 1e-12);
asserts.isequal(stats.sumranks, [10 28 28]);
asserts.isequal(stats.df, 2);
asserts.isequal(stats.source, 'kruskalwallis');
%=============================================================================
y = [1 2 3 4 NaN 1.5 2.5 3.5 5 6]';
g = {'b', 'b', 'a', 'a', 'a', 'b', 'b', 'a', 'c', 'c'}';
[p2, tbl2, stats2] = kruskalwallis(y, g, 'off');
asserts.isapprox(p2, 0.030197383422319, 1e-12);
asserts.isapprox(tbl2{2, 2}, 52.500000000000000, 1e-12);
asserts.isapprox(tbl2{3, 2}, 7.500000000000000, 1e-12);
asserts.isapprox(tbl2{4, 2}, 60.000000000000000, 1e-12);
asserts.isapprox(tbl2{2, 4}, 7.000000000000000, 1e-12);
asserts.isequal(stats2.gnames, {'a'; 'b'; 'c'});
asserts.isequal(stats2.n, [3 4 2]);
asserts.isapprox(stats2.meanranks, [6.000000000000000 2.500000000000000 8.500000000000000], 1e-12);
%=============================================================================
[p3, tbl3, stats3] = kruskalwallis([1 1 1; 1 1 1], [], 'off');
asserts.isequal(p3, 1);
asserts.isequal(tbl3{2, 2}, 0);
asserts.isequal(tbl3{2, 4}, 0);
asserts.isapprox(stats3.meanranks, [3.5 3.5 3.5], 1e-12);
%=============================================================================
[p4, ~, stats4] = kruskalwallis([1 2; 3 NaN; 5 7], ['left '; 'right'], 'off');
asserts.isequal(stats4.gnames, {'left'; 'right'});
asserts.isequal(stats4.n, [3 2]);
asserts.istrue(p4 >= 0 && p4 <= 1);
%=============================================================================
[ps, ~, statss] = kruskalwallis(single(X), [], 'off');
asserts.isequal(class(ps), 'double');
asserts.isapprox(ps, p, 1e-12);
asserts.isapprox(statss.meanranks, stats.meanranks, 1e-12);
%=============================================================================
asserts.checkerror('kruskalwallis()', _('Not enough input arguments.'));
asserts.checkerror('kruskalwallis({1}, [], ''off'')', sprintf(_('%s must be a real numeric vector or matrix.'), 'X'));
asserts.checkerror('kruskalwallis([1 2 3], [1 2], ''off'')', _('GROUP must have the expected number of labels.'));
asserts.checkerror('kruskalwallis([1 2; 3 4], {''only''}, ''off'')', _('GROUP must have the expected number of labels.'));
asserts.checkerror('kruskalwallis([1 2; 3 4], {1}, ''off'')', sprintf(_('%s must be a vector or text array.'), 'GROUP'));
asserts.checkerror('kruskalwallis([1 2; 3 4], [], ''bad'')', _('Display option must be ''on'' or ''off''.'));
asserts.checkerror('kruskalwallis([1 NaN], [1 2], ''off'')', _('At least two groups with more observations than groups are required.'));
%=============================================================================
