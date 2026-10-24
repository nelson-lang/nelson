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
x = exprnd(2, 100000, 1);
tic();
[phat, pci] = expfit(x);
[nlogL, avar] = explike(phat, x);
t = toc();
asserts.istrue(isfinite(phat));
asserts.istrue(all(isfinite(pci(:))));
asserts.istrue(isfinite(nlogL));
asserts.istrue(isfinite(avar));
asserts.istrue(t < 10);
%=============================================================================
