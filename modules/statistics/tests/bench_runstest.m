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
rng(0);
x = randn(250000, 1);
tic();
[h, p, stats] = runstest(x, 0, 'Method', 'approximate');
t = toc();
asserts.istrue(islogical(h));
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(isfield(stats, 'nruns'));
asserts.istrue(isfield(stats, 'z'));
asserts.istrue(t < 10);
%=============================================================================
y = cumsum(randn(150000, 1));
tic();
[h2, p2, stats2] = runstest(y, 'ud');
t2 = toc();
asserts.istrue(islogical(h2));
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.istrue(isfinite(stats2.z));
asserts.istrue(t2 < 10);
%=============================================================================
