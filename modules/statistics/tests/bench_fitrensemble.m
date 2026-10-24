%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 64-->
% <--REUSE PROCESS-->
%=============================================================================
X = randn(180, 4);
y = 3 * X(:, 1) - 2 * X(:, 2) + X(:, 3) .^ 2 + 0.1 * randn(size(X, 1), 1);
Q = randn(80, 4);
tic();
mdl = fitrensemble(X, y, 'NumLearningCycles', 4, 'MaxNumSplits', 8, 'MinLeafSize', 5);
yfit = predict(mdl, Q);
t = toc();
asserts.isequal(size(yfit), [80 1]);
asserts.istrue(mdl.ResubstitutionLoss >= 0);
asserts.istrue(t >= 0);
%=============================================================================
