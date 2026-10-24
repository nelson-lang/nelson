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
rng(0);
x = randn(20000, 1);
y = randn(20000, 1) + 0.02;
tic();
[p, h, stats] = ranksum(x, y, 'Method', 'approximate');
t = toc();
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(islogical(h));
asserts.istrue(isfield(stats, 'ranksum'));
asserts.istrue(isfield(stats, 'zval'));
asserts.istrue(t < 10);
%=============================================================================
x2 = randi(100, 20000, 1);
y2 = randi(100, 20000, 1);
tic();
[p2, h2, stats2] = ranksum(x2, y2);
t2 = toc();
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.istrue(islogical(h2));
asserts.istrue(isfield(stats2, 'zval'));
asserts.istrue(t2 < 10);
%=============================================================================
