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
x = poissrnd(4, 100000, 1);
tic();
[lambdaHat, lambdaCI] = poissfit(x);
[nlogL, avar] = poisslike(lambdaHat, x);
t = toc();
asserts.istrue(isfinite(lambdaHat));
asserts.istrue(all(isfinite(lambdaCI(:))));
asserts.istrue(isfinite(nlogL));
asserts.istrue(isfinite(avar));
asserts.istrue(t < 10);
%=============================================================================