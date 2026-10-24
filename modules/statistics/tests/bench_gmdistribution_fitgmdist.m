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
rng(123);
X = [randn(1500, 2); randn(1500, 2) + 4; randn(1500, 2) + [-4 3]];
tic();
gm = fitgmdist(X, 3, 'MaxIter', 20, 'TolFun', 1e-5, 'CovarianceType', 'diagonal');
tFit = toc();
tic();
[idx, nlogL, P] = cluster(gm, X);
tCluster = toc();
asserts.isequal(size(gm.mu), [3 2]);
asserts.isequal(size(idx), [size(X, 1) 1]);
asserts.isequal(size(P), [size(X, 1) 3]);
asserts.istrue(isfinite(nlogL));
asserts.istrue(tFit >= 0 && tCluster >= 0);
%=============================================================================
