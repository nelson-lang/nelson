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
X = [randn(350, 4); randn(350, 4) + 3; randn(350, 4) - 3];
Y = [ones(350, 1); 2 * ones(350, 1); 3 * ones(350, 1)];
Q = [randn(150, 4); randn(150, 4) + 3; randn(150, 4) - 3];
tic();
mdl = fitctree(X, Y, 'MaxNumSplits', 12, 'MinLeafSize', 5);
[label, score] = predict(mdl, Q);
t = toc();
asserts.isequal(size(label), [450 1]);
asserts.isequal(size(score), [450 3]);
asserts.istrue(t >= 0);
%=============================================================================
