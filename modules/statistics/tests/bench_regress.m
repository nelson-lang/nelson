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
n = 8000;
p = 12;
X = [ones(n, 1), randn(n, p - 1)];
beta = (1:p)';
y = X * beta + 0.05 * randn(n, 1);
tic();
[b, bint, r, rint, stats] = regress(y, X);
t = toc();
asserts.isequal(size(b), [p 1]);
asserts.isequal(size(bint), [p 2]);
asserts.isequal(size(r), [n 1]);
asserts.isequal(size(rint), [n 2]);
asserts.isequal(size(stats), [1 4]);
asserts.istrue(t >= 0);
%=============================================================================
