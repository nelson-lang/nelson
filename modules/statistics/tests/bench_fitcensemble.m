%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
% <--REUSE PROCESS-->
%=============================================================================
X = [randn(120, 4); randn(120, 4) + 3; randn(120, 4) - 3];
Y = [ones(120, 1); 2 * ones(120, 1); 3 * ones(120, 1)];
Q = [randn(50, 4); randn(50, 4) + 3; randn(50, 4) - 3];
tic();
mdl = fitcensemble(X, Y, 'NumLearningCycles', 8, 'MaxNumSplits', 8, 'MinLeafSize', 4);
[label, score] = predict(mdl, Q);
t = toc();
asserts.isequal(size(label), [150 1]);
asserts.isequal(size(score), [150 3]);
asserts.istrue(t >= 0);
%=============================================================================
