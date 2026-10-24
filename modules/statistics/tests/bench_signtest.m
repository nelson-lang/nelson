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
x = randn(250000, 1) + 0.01;
tic();
[p, h, stats] = signtest(x);
t = toc();
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(islogical(h));
asserts.istrue(isfield(stats, 'sign'));
asserts.istrue(isfield(stats, 'zval'));
asserts.istrue(t < 10);
%=============================================================================
x2 = randn(200000, 1);
y2 = x2 - 0.02 + randn(200000, 1) * 0.1;
tic();
[p2, h2, stats2] = signtest(x2, y2, 'Method', 'approximate', 'Tail', 'right');
t2 = toc();
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.istrue(islogical(h2));
asserts.istrue(isfinite(stats2.zval));
asserts.istrue(t2 < 10);
%=============================================================================
