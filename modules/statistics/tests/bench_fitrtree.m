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
X = randn(500, 5);
y = 3 * X(:, 1) - 2 * X(:, 2) + X(:, 3) .^ 2 + 0.1 * randn(size(X, 1), 1);
Q = randn(150, 5);
tic();
mdl = fitrtree(X, y, 'MaxNumSplits', 12, 'MinLeafSize', 6, 'MinParentSize', 12);
[yfit, node] = predict(mdl, Q);
t = toc();
asserts.isequal(size(yfit), [150 1]);
asserts.isequal(size(node), [150 1]);
asserts.istrue(mdl.ResubstitutionLoss >= 0);
asserts.istrue(t >= 0);
%=============================================================================
