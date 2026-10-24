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
X = randn(2500, 5);
y = 3 * X(:, 1) - 2 * X(:, 2) + 0.5 * X(:, 3) .^ 2;
Q = randn(500, 5);
tic();
mdl = fitrknn(X, y, 'NumNeighbors', 5, 'DistanceWeight', 'inverse', 'Standardize', true);
[yfit, D] = predict(mdl, Q);
t = toc();
asserts.isequal(size(yfit), [500 1]);
asserts.isequal(size(D), [500 5]);
asserts.istrue(t >= 0);
%=============================================================================
