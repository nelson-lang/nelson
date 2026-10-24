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
asserts.isequal(nargin('signrank'), -3);
asserts.isequal(nargout('signrank'), 3);
%=============================================================================
[p, h, stats] = signrank([1 3 5 -2]);
asserts.isapprox(p, 0.375000000000000, 1e-12);
asserts.isequal(h, false);
asserts.isequal(stats.signedrank, 8);
asserts.isfalse(isfield(stats, 'zval'));
%=============================================================================
[p2, h2, stats2] = signrank([5 6 7 8], [1 2 3 4], 'Tail', 'right', 'Alpha', 0.1);
asserts.isapprox(p2, 0.035930319112926, 1e-12);
asserts.isequal(h2, true);
asserts.isequal(stats2.signedrank, 10);
asserts.isapprox(stats2.zval, 1.800000000000000, 1e-12);
%=============================================================================
[p3, h3, stats3] = signrank([1 2 2 3 4], 2, 'Method', 'approximate');
asserts.isapprox(p3, 0.586213681073140, 1e-12);
asserts.isequal(h3, false);
asserts.isapprox(stats3.signedrank, 4.500000000000000, 1e-12);
asserts.isapprox(stats3.zval, 0.544331053951817, 1e-12);
%=============================================================================
[p4, h4, stats4] = signrank([1 2 NaN 3], [1 1 1 1], 'Tail', 'right');
asserts.isapprox(p4, 0.250000000000000, 1e-12);
asserts.isequal(h4, false);
asserts.isequal(stats4.signedrank, 3);
%=============================================================================
[ps, hs, statss] = signrank(single([1 3 5 -2]));
asserts.isequal(class(ps), 'double');
asserts.isequal(hs, false);
asserts.isequal(statss.signedrank, stats.signedrank);
%=============================================================================
asserts.checkerror('signrank()', _('Not enough input arguments.'));
asserts.checkerror('signrank({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('signrank([1 2], {1})', sprintf(_('%s must be a real numeric scalar or vector.'), 'Y'));
asserts.checkerror('signrank([1 2], [1 2 3])', sprintf(_('X and Y must have %s.'), 'the same number of observations'));
asserts.checkerror('signrank([1 1 1], [1 1 1])', _('X and Y must contain at least one nonzero non-NaN difference.'));
asserts.checkerror('signrank([1], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('signrank([1], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('signrank([1], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('signrank([1], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
asserts.checkerror('signrank([1 2 2], 0, ''Method'', ''exact'')', _('Exact method is not supported when ties are present.'));
%=============================================================================
