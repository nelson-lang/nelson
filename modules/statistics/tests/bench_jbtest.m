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
x = randn(250000, 1);
tic();
[h, p, jbstat, critval] = jbtest(x);
t = toc();
asserts.istrue(islogical(h));
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(isfinite(jbstat));
asserts.istrue(isfinite(critval));
asserts.istrue(t < 10);
%=============================================================================
y = [randn(200000, 1); 10 * rand(5000, 1)];
tic();
[h2, p2, jbstat2, critval2] = jbtest(y, 0.01);
t2 = toc();
asserts.istrue(islogical(h2));
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.istrue(isfinite(jbstat2));
asserts.istrue(isfinite(critval2));
asserts.istrue(t2 < 10);
%=============================================================================
