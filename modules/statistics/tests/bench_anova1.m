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
rows = 50000;
cols = 12;
X = randn(rows, cols) + repmat(1:cols, rows, 1) / 8;
tic();
[p, tbl, stats] = anova1(X, [], 'off');
t = toc();
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.isequal(size(tbl), [4 6]);
asserts.isequal(length(stats.n), cols);
asserts.istrue(t < 10);
%=============================================================================
g = repmat((1:20)', rows / 20, 1);
y = randn(rows, 1) + g / 20;
tic();
[p2, tbl2, stats2] = anova1(y, g, 'off');
t2 = toc();
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.isequal(size(tbl2), [4 6]);
asserts.isequal(length(stats2.n), 20);
asserts.istrue(t2 < 10);
%=============================================================================
