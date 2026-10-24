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
x = geornd(0.25, 100000, 1);
tic();
[pHat, pCI] = geofit(x);
[nlogL, avar] = geolike(pHat, x);
t = toc();
asserts.istrue(isfinite(pHat));
asserts.istrue(all(isfinite(pCI(:))));
asserts.istrue(isfinite(nlogL));
asserts.istrue(isfinite(avar));
asserts.istrue(t < 10);
%=============================================================================