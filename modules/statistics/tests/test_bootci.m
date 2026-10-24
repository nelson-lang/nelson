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
asserts.isequal(nargin('bootci'), -1);
asserts.isequal(nargout('bootci'), 2);
%=============================================================================
rng('default');
x = (1:10)';
[ci, bootstat] = bootci(40, @mean, x, 'Type', 'percentile');
asserts.isequal(size(ci), [2 1]);
asserts.isequal(size(bootstat), [40 1]);
asserts.istrue(ci(1) <= mean(x));
asserts.istrue(ci(2) >= mean(x));
%=============================================================================
rng(42);
[ci1, bootstat1] = bootci(30, {@bootciTwoStats, x}, 'Type', 'per');
rng(42);
[ci2, bootstat2] = bootci(30, {@bootciTwoStats, x}, 'Type', 'per');
asserts.isequal(ci1, ci2);
asserts.isequal(bootstat1, bootstat2);
asserts.isequal(size(ci1), [2 2]);
asserts.isequal(size(bootstat1), [30 2]);
%=============================================================================
rng(0);
ci = bootci(50, @mean, x, 'Type', 'normal');
asserts.isequal(size(ci), [2 1]);
rng(0);
ci = bootci(50, @mean, x, 'Type', 'cper');
asserts.isequal(size(ci), [2 1]);
rng(0);
ci = bootci(50, @mean, x, 'Type', 'bca');
asserts.isequal(size(ci), [2 1]);
%=============================================================================
rng(1);
ci = bootci(30, {@bootciMeanSum, x, x + 1}, 'Alpha', 0.1, 'Type', 'per');
asserts.isequal(size(ci), [2 1]);
%=============================================================================
rng(1);
ci = bootci(30, {@mean, x}, 'Weights', [ones(9, 1); 10], 'Type', 'per');
asserts.isequal(size(ci), [2 1]);
%=============================================================================
opts = statset('bootci');
asserts.isequal(opts.UseParallel, false);
%=============================================================================
asserts.checkerror('bootci()', _('Not enough input arguments.'));
asserts.checkerror('bootci(0, @mean, x)', sprintf(_('%s must be a positive integer scalar.'), 'NBOOT'));
asserts.checkerror('bootci(10, ''mean'', x)', sprintf(_('%s must be a function handle.'), 'BOOTFUN'));
asserts.checkerror('bootci(10, @mean, 1)', sprintf(_('At least one %s is required.'), 'nonscalar data argument'));
asserts.checkerror('bootci(10, @mean, x, ''Alpha'', 2)', _('Alpha must be a scalar between 0 and 1.'));
asserts.checkerror('bootci(10, @mean, x, ''Type'', ''bad'')', sprintf(_('Invalid %s value.'), 'Type'));
asserts.checkerror('bootci(10, @mean, x, ''Type'', ''student'')', _('Studentized bootstrap confidence intervals are not yet supported.'));
asserts.checkerror('bootci(10, @mean, x, ''Weights'', ones(2, 1))', _('Weights must be a nonnegative vector with one value per observation.'));
%=============================================================================
function y = bootciTwoStats(x)
  y = [mean(x) std(x)];
end
%=============================================================================
function y = bootciMeanSum(a, b)
  y = mean(a + b);
end
%=============================================================================
