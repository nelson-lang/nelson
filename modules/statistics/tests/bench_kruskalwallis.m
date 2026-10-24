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
rows = 30000;
groups = 8;
X = randn(rows, groups) + repmat(1:groups, rows, 1) / 10;
tic();
[p, tbl, stats] = kruskalwallis(X, [], 'off');
t = toc();
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.isequal(size(tbl), [4 5]);
asserts.isequal(length(stats.n), groups);
asserts.istrue(t < 10);
%=============================================================================
n = 60000;
y = randn(n, 1);
g = repmat((1:12)', n / 12, 1);
tic();
[p2, tbl2, stats2] = kruskalwallis(y, g, 'off');
t2 = toc();
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.isequal(size(tbl2), [4 5]);
asserts.isequal(length(stats2.n), 12);
asserts.istrue(t2 < 10);
%=============================================================================
