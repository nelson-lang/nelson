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
asserts.isequal(nargin('lillietest'), -2);
asserts.isequal(nargout('lillietest'), 4);
%=============================================================================
[h, p, kstat, critval] = lillietest([-1 -0.5 0 0.5 1]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.5, 1e-12);
asserts.isapprox(kstat, 0.136455371567231, 1e-12);
asserts.isapprox(critval, 0.343821551051317, 1e-12);
%=============================================================================
[h2, p2, kstat2, critval2] = lillietest([0.1 0.2 0.4 0.9 1.6], ...
  'Distribution', 'exponential');
asserts.isequal(h2, false);
asserts.isapprox(p2, 0.5, 1e-12);
asserts.isapprox(kstat2, 0.154939460754474, 1e-12);
asserts.isapprox(critval2, critval, 1e-12);
%=============================================================================
[h3, p3, kstat3, critval3] = lillietest([-2 -1 0 1 2], ...
  'Distr', 'ev', 'Alpha', 0.1);
asserts.isequal(h3, true);
asserts.isapprox(p3, 0.003110438955619, 1e-12);
asserts.isapprox(kstat3, 0.431542606375316, 1e-12);
asserts.isapprox(critval3, 0.317306365956473, 1e-12);
%=============================================================================
[h4, p4, kstat4, critval4] = lillietest(single([-1 -0.5 NaN 0 0.5 1]), ...
  'MCTol', 0.01, 'Alpha', 0.2);
asserts.isequal(h4, false);
asserts.isequal(class(p4), 'double');
asserts.isapprox(p4, 0.5, 1e-12);
asserts.isapprox(kstat4, kstat, 1e-12);
asserts.isapprox(critval4, 0.284993850974512, 1e-12);
%=============================================================================
[h5, p5, kstat5, critval5] = lillietest([zeros(1, 20) 100]);
asserts.isequal(h5, true);
asserts.isapprox(p5, 0.001, 1e-12);
asserts.isapprox(kstat5, 0.538751279099597, 1e-12);
asserts.isapprox(critval5, 0.188326207784628, 1e-12);
%=============================================================================
asserts.checkerror('lillietest()', _('Not enough input arguments.'));
asserts.checkerror('lillietest({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('lillietest([1 2 NaN])', _('X must contain at least four non-NaN observations.'));
asserts.checkerror('lillietest([1 2 3 4], ''Alpha'', 0.8)', _('ALPHA must be in the range [0.001,0.5] when MCTOL is not specified.'));
asserts.checkerror('lillietest([1 2 3 4], ''MCTol'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'MCTOL'));
asserts.checkerror('lillietest([1 2 3 4], ''Distribution'', ''gamma'')', sprintf(_('%s has an unsupported value.'), 'DISTRIBUTION'));
asserts.checkerror('lillietest([-1 1 2 3], ''Distribution'', ''exponential'')', _('X must be nonnegative for the exponential distribution.'));
asserts.checkerror('lillietest([1 2 3 4], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('lillietest([1 2 3 4], ''Unknown'', ''normal'')', _('Invalid name-value argument name.'));
%=============================================================================
