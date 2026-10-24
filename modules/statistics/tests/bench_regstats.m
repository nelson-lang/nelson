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
n = 6000;
p = 8;
X = randn(n, p);
beta = (1:p)';
y = 1.5 + X * beta + 0.05 * randn(n, 1);
tic();
stats = regstats(y, X, 'linear', {'beta', 'yhat', 'r', 'mse', 'rsquare'});
t = toc();
asserts.isequal(size(stats.beta), [p + 1 1]);
asserts.isequal(size(stats.yhat), [n 1]);
asserts.isequal(size(stats.r), [n 1]);
asserts.istrue(t >= 0);
%=============================================================================
n2 = 700;
X2 = X(1:n2, 1:5);
y2 = y(1:n2);
tic();
infl = regstats(y2, X2, 'linear', {'leverage', 'standres', 'studres', 'cookd'});
t2 = toc();
asserts.isequal(size(infl.leverage), [n2 1]);
asserts.isequal(size(infl.standres), [n2 1]);
asserts.isequal(size(infl.studres), [n2 1]);
asserts.isequal(size(infl.cookd), [n2 1]);
asserts.istrue(t2 >= 0);
%=============================================================================
