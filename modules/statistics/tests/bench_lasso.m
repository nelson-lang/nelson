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
n = 1200;
p = 20;
X = randn(n, p);
beta = zeros(p, 1);
beta([2 7 13]) = [2; -3; 1.5];
y = X * beta + 0.1 * randn(n, 1);
tic();
[B, FitInfo] = lasso(X, y, 'NumLambda', 25);
t = toc();
asserts.isequal(size(B), [p 25]);
asserts.isequal(length(FitInfo.Lambda), 25);
asserts.istrue(t >= 0);
%=============================================================================
