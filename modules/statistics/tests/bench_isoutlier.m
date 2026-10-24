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
X = randn(20000, 12);
X(100:2500:end, :) = X(100:2500:end, :) * 20;
tic();
[TF, L, U, C] = isoutlier(X, 'median');
t = toc();
asserts.isequal(size(TF), size(X));
asserts.isequal(size(L), [1 12]);
asserts.isequal(size(U), [1 12]);
asserts.isequal(size(C), [1 12]);
asserts.istrue(t >= 0);
%=============================================================================
Y = randn(2000, 8);
Y(20:200:end, :) = 12;
tic();
TF = isoutlier(Y, 'movmean', 11);
t = toc();
asserts.isequal(size(TF), size(Y));
asserts.istrue(t >= 0);
%=============================================================================
