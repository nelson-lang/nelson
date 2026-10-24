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
asserts.isequal(nargin('bootstrp'), -1);
asserts.isequal(nargout('bootstrp'), 2);
%=============================================================================
rng('default');
x = (1:10)';
[bootstat, bootsam] = bootstrp(25, @mean, x);
asserts.isequal(size(bootstat), [25 1]);
asserts.isequal(size(bootsam), [10 25]);
asserts.istrue(all(bootsam(:) >= 1));
asserts.istrue(all(bootsam(:) <= 10));
%=============================================================================
rng(42);
[bootstat1, bootsam1] = bootstrp(20, @bootstrpTwoStats, x);
rng(42);
[bootstat2, bootsam2] = bootstrp(20, @bootstrpTwoStats, x);
asserts.isequal(bootstat1, bootstat2);
asserts.isequal(bootsam1, bootsam2);
asserts.isequal(size(bootstat1), [20 2]);
%=============================================================================
rng(1);
bootstat = bootstrp(15, @bootstrpMeanSum, x, x + 1);
asserts.isequal(size(bootstat), [15 1]);
%=============================================================================
rng(2);
[bootstat, bootsam] = bootstrp(12, [], x);
asserts.isequal(bootstat, []);
asserts.isequal(size(bootsam), [10 12]);
%=============================================================================
rng(3);
[bootstat, bootsam] = bootstrp(18, @mean, 1:10);
asserts.isequal(size(bootstat), [18 1]);
asserts.isequal(size(bootsam), [10 18]);
%=============================================================================
rng(4);
bootstat = bootstrp(20, @mean, x, 'Weights', [ones(9, 1); 10]);
asserts.isequal(size(bootstat), [20 1]);
%=============================================================================
opts = statset('bootstrp');
asserts.isequal(opts.UseParallel, false);
opts = statset(opts, 'UseSubstreams', true);
bootstat = bootstrp(5, @mean, x, 'Options', opts);
asserts.isequal(size(bootstat), [5 1]);
%=============================================================================
asserts.checkerror('bootstrp()', _('Not enough input arguments.'));
asserts.checkerror('bootstrp(0, @mean, x)', sprintf(_('%s must be a positive integer scalar.'), 'NBOOT'));
asserts.checkerror('bootstrp(10, ''mean'', x)', _('BOOTFUN must be empty or a function handle.'));
asserts.checkerror('bootstrp(10, @mean, 1)', sprintf(_('At least one %s is required.'), 'nonscalar data argument'));
asserts.checkerror('bootstrp(10, @mean, x, ''Weights'', ones(2, 1))', _('Weights must be a nonnegative vector with one value per observation.'));
asserts.checkerror('bootstrp(10, @mean, x, ''Options'', 1)', _('Options must be a structure.'));
asserts.checkerror('bootstrp(10, @mean, x, ''Unknown'', 1)', _('Invalid name-value argument name.'));
%=============================================================================
function y = bootstrpTwoStats(x)
  y = [mean(x) std(x)];
end
%=============================================================================
function y = bootstrpMeanSum(a, b)
  y = mean(a + b);
end
%=============================================================================
