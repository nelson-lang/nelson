%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
% <--REUSE PROCESS-->
%=============================================================================
rng(0);
blocks = 5000;
treatments = 8;
X = randn(blocks, treatments) + repmat(1:treatments, blocks, 1) / 10;
tic();
[p, tbl, stats] = friedman(X, 'off');
t = toc();
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.isequal(size(tbl), [4 5]);
asserts.isequal(length(stats.meanranks), treatments);
asserts.istrue(t < 10);
%=============================================================================
reps = 3;
Y = randn(blocks * reps, treatments) + repmat(1:treatments, blocks * reps, 1) / 10;
tic();
[p2, tbl2, stats2] = friedman(Y, reps, 'off');
t2 = toc();
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.isequal(size(tbl2), [4 5]);
asserts.isequal(stats2.reps, reps);
asserts.istrue(t2 < 10);
%=============================================================================
