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
rowLevels = 300;
colLevels = 24;
reps = 4;
baseRows = repmat((1:rowLevels)', reps, 1) / 300;
baseCols = (1:colLevels) / 50;
Y = randn(rowLevels * reps, colLevels) + repmat(baseRows, 1, colLevels) + repmat(baseCols, rowLevels * reps, 1);
tic();
[p, tbl, stats] = anova2(Y, reps, 'off');
t = toc();
asserts.isequal(length(p), 3);
asserts.isequal(size(tbl), [6 6]);
asserts.isequal(size(stats.cellmeans), [rowLevels colLevels]);
asserts.istrue(all(isfinite(p)));
asserts.istrue(t < 10);
%=============================================================================
Z = randn(2000, 16) + repmat((1:2000)', 1, 16) / 2000 + repmat((1:16), 2000, 1) / 20;
tic();
[p2, tbl2, stats2] = anova2(Z, 'off');
t2 = toc();
asserts.isequal(length(p2), 2);
asserts.isequal(size(tbl2), [5 6]);
asserts.isequal(length(stats2.rowmeans), 2000);
asserts.istrue(all(isfinite(p2)));
asserts.istrue(t2 < 10);
%=============================================================================
