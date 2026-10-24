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
asserts.isequal(nargin('adtest'), -2);
asserts.isequal(nargout('adtest'), 4);
%=============================================================================
[h, p, adstat, cv] = adtest([1 2 3 4 5]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.919582303387710, 1e-12);
asserts.isapprox(adstat, 0.143594203672524, 1e-12);
asserts.isapprox(cv, 0.607414449395958, 1e-12);
%=============================================================================
[h2, p2, adstat2, cv2] = adtest([0.2 0.5 1.2 2.0 3.5], ...
  'Distribution', 'exp');
asserts.isequal(h2, false);
asserts.isapprox(p2, 0.888268211707402, 1e-12);
asserts.isapprox(adstat2, 0.159026908718809, 1e-12);
asserts.isapprox(cv2, cv, 1e-12);
%=============================================================================
[h3, p3, adstat3, cv3] = adtest([-2 -1 0 1 2], ...
  'Distribution', 'ev', 'Alpha', 0.1);
asserts.isequal(h3, true);
asserts.istrue(p3 > 0 && p3 < 1e-8);
asserts.isapprox(adstat3, 3.546888106839127, 1e-12);
asserts.isapprox(cv3, 0.509057049824038, 1e-12);
%=============================================================================
[h4, p4, adstat4, cv4] = adtest([1 2 4 8 16], ...
  'Distribution', 'logn', 'Asymptotic', true);
asserts.isequal(h4, false);
asserts.isapprox(p4, 0.970578723749282, 1e-12);
asserts.isapprox(adstat4, 0.143594203672524, 1e-12);
asserts.isapprox(cv4, 0.753193917250988, 1e-12);
%=============================================================================
[h5, p5, adstat5, cv5] = adtest([1 2 3 5 8], ...
  'Distribution', 'weibull', 'MCTol', 0.01);
asserts.isequal(h5, true);
asserts.istrue(p5 > 0 && p5 < 1e-8);
asserts.isapprox(adstat5, 3.556273795468238, 1e-12);
asserts.isapprox(cv5, cv, 1e-12);
%=============================================================================
[h6, p6, adstat6, cv6] = adtest(single([1 2 NaN 3 4 5]));
asserts.isequal(h6, false);
asserts.isequal(class(p6), 'double');
asserts.isapprox(p6, p, 1e-12);
asserts.isapprox(adstat6, adstat, 1e-12);
asserts.isapprox(cv6, cv, 1e-12);
%=============================================================================
[h7, p7, adstat7, cv7] = adtest([zeros(1, 20) 100]);
asserts.isequal(h7, true);
asserts.istrue(p7 > 0 && p7 < 1e-16);
asserts.isapprox(adstat7, 7.569082917062044, 1e-12);
asserts.isapprox(cv7, 0.723656900888204, 1e-12);
%=============================================================================
asserts.checkerror('adtest()', _('Not enough input arguments.'));
asserts.checkerror('adtest({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('adtest([1 NaN])', _('X must contain at least three non-NaN observations.'));
asserts.checkerror('adtest([1 2 3], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('adtest([1 2 3], ''Distribution'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'DISTRIBUTION'));
asserts.checkerror('adtest([-1 1 2], ''Distribution'', ''exp'')', _('X must be nonnegative for the exponential distribution.'));
asserts.checkerror('adtest([0 1 2], ''Distribution'', ''logn'')', sprintf(_('X must be positive for the %s distribution.'), 'lognormal'));
asserts.checkerror('adtest([0 1 2], ''Distribution'', ''weibull'')', sprintf(_('X must be positive for the %s distribution.'), 'Weibull'));
asserts.checkerror('adtest([1 2 3], ''MCTol'', 0.01, ''Asymptotic'', true)', _('ASYMPTOTIC must be false when MCTOL is specified.'));
asserts.checkerror('adtest([1 2 3], ''Alpha'')', _('Name-value arguments must be paired.'));
%=============================================================================
