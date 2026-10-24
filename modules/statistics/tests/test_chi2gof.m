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
asserts.isequal(nargin('chi2gof'), -2);
asserts.isequal(nargout('chi2gof'), 3);
%=============================================================================
x = norminv(((1:100) - 0.5) / 100);
[h, p, stats] = chi2gof(x);
asserts.isequal(h, false);
asserts.isequal(p, 1);
asserts.istrue(abs(stats.chi2stat) < 1e-8);
asserts.isequal(stats.df, 9);
asserts.isequal(stats.O, 10 * ones(1, 10));
asserts.isapprox(stats.E, 10 * ones(1, 10), 1e-12);
%=============================================================================
[h1, p1, stats1] = chi2gof([1 1 1 2 2 3], ...
  'Edges', [0.5 1.5 2.5 3.5], 'Expected', [2 2 2], 'EMin', 0.1);
asserts.isequal(h1, false);
asserts.isapprox(p1, 0.606530659712633, 1e-12);
asserts.isapprox(stats1.chi2stat, 1, 1e-12);
asserts.isequal(stats1.df, 2);
asserts.isequal(stats1.O, [3 2 1]);
asserts.isequal(stats1.E, [2 2 2]);
%=============================================================================
[h2, p2, stats2] = chi2gof([1 2 3], 'Frequency', [3 2 1], ...
  'Edges', [0.5 1.5 2.5 3.5], 'Expected', [2 2 2], 'EMin', 0.1);
asserts.isequal(h2, false);
asserts.isapprox(p2, 0.606530659712633, 1e-12);
asserts.isapprox(stats2.chi2stat, 1, 1e-12);
asserts.isequal(stats2.O, [3 2 1]);
%=============================================================================
sample = [-2 -1 -0.5 0 0.5 1 2];
[h3, p3, stats3] = chi2gof(sample, 'Edges', [-Inf -1 0 1 Inf], ...
  'CDF', @(z) normcdf(z), 'EMin', 0.1);
asserts.isequal(h3, false);
asserts.isapprox(p3, 0.837419473808862, 1e-12);
asserts.isapprox(stats3.chi2stat, 0.850226893029783, 1e-12);
asserts.isequal(stats3.df, 3);
asserts.isequal(stats3.O, [2 2 2 1]);
%=============================================================================
cdf = [-2 0; -1 0.25; 0 0.5; 1 0.75; 2 1];
[h4, p4, stats4] = chi2gof(sample, 'Edges', [-Inf -1 0 1 Inf], ...
  'CDF', cdf, 'EMin', 0.1);
asserts.isequal(h4, false);
asserts.isapprox(p4, 0.934279075624251, 1e-12);
asserts.isapprox(stats4.chi2stat, 0.428571428571429, 1e-12);
asserts.isequal(stats4.df, 3);
%=============================================================================
[h5, p5, stats5] = chi2gof(single([1 1 2 3 3 3]), 'Ctrs', [1 2 3], ...
  'Expected', [2 2 2], 'EMin', 0.1, 'Alpha', 0.2);
asserts.isequal(h5, false);
asserts.isapprox(p5, 0.606530659712633, 1e-12);
asserts.isapprox(stats5.chi2stat, 1, 1e-12);
asserts.isequal(stats5.df, 2);
%=============================================================================
[h6, p6, stats6] = chi2gof([1 NaN Inf 2 3], 'Frequency', [1 1 1 1 1], ...
  'Edges', [0.5 1.5 2.5 3.5], 'Expected', [1 1 1], 'EMin', 0.1);
asserts.isequal(h6, false);
asserts.isequal(p6, 1);
asserts.isequal(stats6.O, [1 1 1]);
%=============================================================================
asserts.checkerror('chi2gof()', _('Not enough input arguments.'));
asserts.checkerror('chi2gof({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('chi2gof([NaN Inf])', _('X must contain at least one finite observation.'));
asserts.checkerror('chi2gof([1 2], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('chi2gof([1 2], ''NBins'', 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'NBINS'));
asserts.checkerror('chi2gof([1 2], ''Edges'', [0 1 1])', sprintf(_('%s must be a strictly increasing vector.'), 'EDGES'));
asserts.checkerror('chi2gof([1 2], ''Expected'', [-1 2])', _('EXPECTED must contain nonnegative counts with positive sum.'));
asserts.checkerror('chi2gof([1 2], ''Frequency'', [1 2 3])', sprintf(_('%s must have one value per observation.'), 'FREQUENCY'));
asserts.checkerror('chi2gof([1 2], ''CDF'', [0 0.5 1])', _('CDF must be a two-column matrix or function handle.'));
asserts.checkerror('chi2gof([1 2], ''Unknown'', 1)', _('Invalid name-value argument name.'));
%=============================================================================
