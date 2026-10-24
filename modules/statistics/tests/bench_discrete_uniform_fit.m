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
x = unidrnd(100, 1000000, 1);
tic();
[nHat, nCI] = unidfit(x);
[nlogL, avar] = unidlike(nHat, x);
t = toc();
asserts.isequal(nHat, 100);
asserts.istrue(all(isfinite(nCI(:))));
asserts.istrue(isfinite(nlogL));
asserts.istrue(isnan(avar));
asserts.istrue(t < 10);
%=============================================================================
