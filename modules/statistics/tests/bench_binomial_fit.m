%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 64-->
% <--REUSE PROCESS-->
%=============================================================================
rng(0);
n = 20 * ones(100000, 1);
x = binornd(n, 0.35);
tic();
[pHat, pCI] = binofit(x, n);
[nlogL, avar] = binolike(mean(pHat), x, n);
t = toc();
asserts.istrue(all(isfinite(pHat(:))));
asserts.istrue(all(isfinite(pCI(:))));
asserts.istrue(isfinite(nlogL));
asserts.istrue(isfinite(avar));
asserts.istrue(t < 10);
%=============================================================================
