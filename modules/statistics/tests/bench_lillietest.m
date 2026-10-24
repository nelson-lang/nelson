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
x = randn(200000, 1);
tic();
[h, p, kstat, critval] = lillietest(x);
t = toc();
asserts.istrue(islogical(h));
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(isfinite(kstat));
asserts.istrue(isfinite(critval));
asserts.istrue(t < 10);
%=============================================================================
y = exprnd(2, 100000, 1);
tic();
[h2, p2, kstat2, critval2] = lillietest(y, 'Distribution', 'exponential');
t2 = toc();
asserts.istrue(islogical(h2));
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.istrue(isfinite(kstat2));
asserts.istrue(isfinite(critval2));
asserts.istrue(t2 < 10);
%=============================================================================
