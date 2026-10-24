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
asserts.isequal(nargin('ansaribradley'), -3);
asserts.isequal(nargout('ansaribradley'), 3);
%=============================================================================
[h, p, stats] = ansaribradley([1 2 9 10], [4 5 6 7]);
asserts.isequal(h, true);
asserts.isapprox(p, 0.028571428571429, 1e-12);
asserts.isequal(stats.W, 6);
asserts.isapprox(stats.Wstar, -2.366431913239846, 1e-12);
%=============================================================================
[h2, p2, stats2] = ansaribradley([1 2 9 10], [4 5 6 7], ...
  'Tail', 'right', 'Alpha', 0.2);
asserts.isequal(h2, true);
asserts.isapprox(p2, 0.014285714285714, 1e-12);
asserts.isequal(stats2.W, 6);
asserts.isapprox(stats2.Wstar, stats.Wstar, 1e-12);
%=============================================================================
[h3, p3, stats3] = ansaribradley(1:10, (1:10) + 0.1, 'Method', 'approximate');
asserts.isequal(h3, false);
asserts.isequal(p3, 1);
asserts.isequal(stats3.W, 55);
asserts.isequal(stats3.Wstar, 0);
%=============================================================================
X = [1 2; 3 4; 9 6; 10 8];
Y = [4 1; 5 3; 6 9; 7 10];
[h4, p4, stats4] = ansaribradley(X, Y, 'Dim', 1);
asserts.isequal(h4, [true false]);
asserts.isapprox(p4, [0.028571428571429 0.142857142857143], 1e-12);
asserts.isequal(stats4.W, [6 13]);
asserts.isapprox(stats4.Wstar, [-2.366431913239846 1.774823934929885], 1e-12);
%=============================================================================
[hs, ps, statss] = ansaribradley(single([1 2 9 10]), single([4 5 6 7]));
asserts.isequal(hs, true);
asserts.isequal(class(ps), 'double');
asserts.isapprox(ps, p, 1e-12);
asserts.isequal(statss.W, stats.W);
%=============================================================================
asserts.checkerror('ansaribradley([1 2])', _('Not enough input arguments.'));
asserts.checkerror('ansaribradley({1}, [1])', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('ansaribradley([1], {1})', sprintf(_('%s must be a real numeric array.'), 'Y'));
asserts.checkerror('ansaribradley([NaN], [1])', sprintf(_('%s must contain at least one non-NaN observation.'), 'X and Y'));
asserts.checkerror('ansaribradley(ones(2,2), ones(3,3))', sprintf(_('X and Y must have %s.'), 'compatible sizes outside the test dimension'));
asserts.checkerror('ansaribradley([1], [2], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('ansaribradley([1], [2], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('ansaribradley([1], [2], ''Unknown'', 1)', _('Invalid name-value argument name.'), 'Nelson:statistics:unknownNameValue');
asserts.checkerror('ansaribradley([1], [2], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('ansaribradley([1], [2], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
asserts.checkerror('ansaribradley([1], [2], ''Method'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'METHOD'));
asserts.checkerror('ansaribradley([1 1], [2 2], ''Method'', ''exact'')', _('Exact method is not supported when ties are present.'));
%=============================================================================
