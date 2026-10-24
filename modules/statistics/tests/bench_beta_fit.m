%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--REUSE PROCESS-->
%=============================================================================
rng(0);
x = betarnd(2, 5, 100000, 1);
tic();
[phat, pci] = betafit(x);
[nlogL, avar] = betalike(phat, x);
t = toc();
asserts.istrue(all(isfinite(phat(:))));
asserts.istrue(all(isfinite(pci(:))));
asserts.istrue(isfinite(nlogL));
asserts.istrue(all(isfinite(avar(:))));
asserts.istrue(t < 10);
%=============================================================================