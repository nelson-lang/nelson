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
asserts.isequal(nargin('ranksum'), -3);
asserts.isequal(nargout('ranksum'), 3);
%=============================================================================
[p, h, stats] = ranksum([1 3 5], [2 4 6]);
asserts.isapprox(p, 0.700000000000000, 1e-12);
asserts.isequal(h, false);
asserts.isequal(stats.ranksum, 9);
asserts.isfalse(isfield(stats, 'zval'));
%=============================================================================
[p2, h2, stats2] = ranksum([1 2 2 3 4], [2 3 4 4 5], 'Method', 'approximate');
asserts.isapprox(p2, 0.162586850726355, 1e-12);
asserts.isequal(h2, false);
asserts.isapprox(stats2.ranksum, 20.500000000000000, 1e-12);
asserts.isapprox(stats2.zval, 1.396424004376894, 1e-12);
%=============================================================================
[p3, h3, stats3] = ranksum([5 6 7 8], [1 2 3 4], 'Tail', 'right', 'Alpha', 0.1);
asserts.isapprox(p3, 0.014285714285714, 1e-12);
asserts.isequal(h3, true);
asserts.isequal(stats3.ranksum, 26);
%=============================================================================
[p4, h4, stats4] = ranksum([1 2 NaN 3], [3 4 5], 'Tail', 'left');
asserts.isapprox(p4, 0.060591636418732, 1e-12);
asserts.isequal(h4, false);
asserts.isapprox(stats4.ranksum, 6.500000000000000, 1e-12);
asserts.isapprox(stats4.zval, -1.549826049695167, 1e-12);
%=============================================================================
[p5, h5, stats5] = ranksum([1 1 1], [1 1 1]);
asserts.isequal(p5, 1);
asserts.isequal(h5, false);
asserts.isapprox(stats5.ranksum, 10.500000000000000, 1e-12);
asserts.isequal(stats5.zval, 0);
%=============================================================================
[ps, hs, statss] = ranksum(single([1 3 5]), single([2 4 6]));
asserts.isequal(class(ps), 'double');
asserts.isequal(hs, false);
asserts.isequal(statss.ranksum, stats.ranksum);
%=============================================================================
asserts.checkerror('ranksum()', _('Not enough input arguments.'));
asserts.checkerror('ranksum({1}, [1 2])', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('ranksum([1 2], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('ranksum([NaN], [1])', sprintf(_('%s must contain at least one non-NaN observation.'), 'X and Y'));
asserts.checkerror('ranksum([1], [1], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('ranksum([1], [1], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('ranksum([1], [1], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('ranksum([1], [1], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
asserts.checkerror('ranksum([1 1], [1 1], ''Method'', ''exact'')', _('Exact method is not supported when ties are present.'));
%=============================================================================
