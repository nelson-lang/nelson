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
asserts.isequal(nargin('signtest'), -3);
asserts.isequal(nargout('signtest'), 3);
%=============================================================================
[p, h, stats] = signtest([1 2 -3 4 -5]);
asserts.isapprox(p, 1, 1e-12);
asserts.isequal(h, false);
asserts.isequal(stats.sign, 3);
asserts.istrue(isnan(stats.zval));
%=============================================================================
[p2, h2, stats2] = signtest([5 6 7 8], [1 2 3 4], 'Tail', 'right', 'Alpha', 0.1);
asserts.isapprox(p2, 0.062500000000000, 1e-12);
asserts.isequal(h2, true);
asserts.isequal(stats2.sign, 4);
asserts.istrue(isnan(stats2.zval));
%=============================================================================
[p3, h3, stats3] = signtest([1 2 2 3 4], 2, 'Method', 'approximate');
asserts.isapprox(p3, 1, 1e-12);
asserts.isequal(h3, false);
asserts.isequal(stats3.sign, 2);
asserts.isequal(stats3.zval, 0);
%=============================================================================
[p4, h4, stats4] = signtest([1 2 NaN 3], [1 1 1 1], 'Tail', 'right');
asserts.isapprox(p4, 0.250000000000000, 1e-12);
asserts.isequal(h4, false);
asserts.isequal(stats4.sign, 2);
asserts.istrue(isnan(stats4.zval));
%=============================================================================
x = ones(1, 110);
[p5, h5, stats5] = signtest(x, 0, 'Method', 'approximate', 'Tail', 'right');
asserts.istrue(p5 > 0 && p5 < 1e-20);
asserts.isequal(h5, true);
asserts.isequal(stats5.sign, 110);
asserts.isapprox(stats5.zval, 10.392742222776956, 1e-12);
%=============================================================================
[ps, hs, statss] = signtest(single([1 2 -3 4 -5]));
asserts.isequal(class(ps), 'double');
asserts.isequal(hs, false);
asserts.isequal(statss.sign, stats.sign);
asserts.istrue(isnan(statss.zval));
%=============================================================================
asserts.checkerror('signtest()', _('Not enough input arguments.'));
asserts.checkerror('signtest({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('signtest([1 2], {1})', sprintf(_('%s must be a real numeric scalar or vector.'), 'Y'));
asserts.checkerror('signtest([1 2], [1 2 3])', sprintf(_('X and Y must have %s.'), 'the same number of observations'));
asserts.checkerror('signtest([NaN 0])', _('X and Y must contain at least one nonzero non-NaN difference.'));
asserts.checkerror('signtest([1], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('signtest([1], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('signtest([1], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('signtest([1], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
asserts.checkerror('signtest([1], ''Method'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'METHOD'));
%=============================================================================
