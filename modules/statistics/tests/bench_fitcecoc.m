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
X = [randn(80, 4); randn(80, 4) + 3; randn(80, 4) - 3];
Y = [ones(80, 1); 2 * ones(80, 1); 3 * ones(80, 1)];
Q = [randn(40, 4); randn(40, 4) + 3; randn(40, 4) - 3];
tic();
mdl = fitcecoc(X, Y, 'KernelFunction', 'linear', 'Standardize', true, 'IterationLimit', 1000);
[label, score] = predict(mdl, Q);
t = toc();
asserts.isequal(size(label), [120 1]);
asserts.isequal(size(score), [120 3]);
asserts.istrue(t >= 0);
%=============================================================================
