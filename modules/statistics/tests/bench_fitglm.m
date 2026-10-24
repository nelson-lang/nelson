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
X = randn(8000, 5);
beta = (1:6)';
y = [ones(size(X, 1), 1) X] * beta + 0.01 * randn(size(X, 1), 1);
tic();
mdl = fitglm(X, y);
yhat = predict(mdl, X(1:1000, :));
t = toc();
asserts.isequal(size(yhat), [1000 1]);
asserts.istrue(mdl.Rsquared.Ordinary > 0.99);
asserts.istrue(t >= 0);
%=============================================================================
Xp = (0:3999)';
yp = max(0, round(exp(0.1 + 0.001 * Xp)));
tic();
mp = fitglm(Xp, yp, 'Distribution', 'poisson');
ypred = predict(mp, Xp(1:1000));
t = toc();
asserts.isequal(size(ypred), [1000 1]);
asserts.istrue(all(ypred > 0));
asserts.istrue(t >= 0);
%=============================================================================
