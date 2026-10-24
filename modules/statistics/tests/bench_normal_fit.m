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
x = normrnd(2, 3, 100000, 1);
tic();
[muhat, sigmahat, muci, sigmaci] = normfit(x);
[nlogL, avar] = normlike([muhat sigmahat], x);
t = toc();
asserts.istrue(isfinite(muhat));
asserts.istrue(isfinite(sigmahat));
asserts.istrue(all(isfinite(muci(:))));
asserts.istrue(all(isfinite(sigmaci(:))));
asserts.istrue(isfinite(nlogL));
asserts.istrue(all(isfinite(avar(:))));
asserts.istrue(t < 10);
%=============================================================================