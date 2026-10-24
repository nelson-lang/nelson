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
X = [randn(120, 4); randn(120, 4) + 3];
Y = [ones(120, 1); 2 * ones(120, 1)];
Q = [randn(80, 4); randn(80, 4) + 3];
tic();
mdl = fitcsvm(X, Y, 'KernelFunction', 'linear', 'Standardize', true, 'IterationLimit', 1000);
[label, score] = predict(mdl, Q);
t = toc();
asserts.isequal(size(label), [160 1]);
asserts.isequal(size(score), [160 2]);
asserts.istrue(t >= 0);
%=============================================================================
