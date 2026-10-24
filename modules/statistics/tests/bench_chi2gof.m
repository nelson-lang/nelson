%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--REUSE PROCESS-->
%=============================================================================
rng(0);
n = 100000;
x = randn(n, 1);
tic();
[h, p, stats] = chi2gof(x, 'NBins', 25);
t = toc();
asserts.istrue(islogical(h));
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(isfield(stats, 'chi2stat'));
asserts.istrue(isfield(stats, 'df'));
asserts.istrue(t < 10);
%=============================================================================
values = (1:20)';
frequency = randi(100, 20, 1);
expected = sum(frequency) / 20 * ones(1, 20);
tic();
[h2, p2, stats2] = chi2gof(values, 'Frequency', frequency, ...
  'Edges', 0.5:1:20.5, 'Expected', expected);
t2 = toc();
asserts.istrue(islogical(h2));
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.isequal(length(stats2.O), 20);
asserts.istrue(t2 < 10);
%=============================================================================
