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
X = randn(10000, 6);
beta = (1:7)';
y = [ones(size(X, 1), 1) X] * beta + 0.01 * randn(size(X, 1), 1);
tic();
mdl = fitlm(X, y);
yhat = predict(mdl, X(1:1000, :));
t = toc();
asserts.isequal(size(yhat), [1000 1]);
asserts.istrue(mdl.Rsquared.Ordinary > 0.99);
asserts.istrue(t >= 0);
%=============================================================================
