%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--REUSE PROCESS-->
%=============================================================================
X = [randn(1500, 4); randn(1500, 4) + 3; randn(1500, 4) - 3];
Y = [ones(1500, 1); 2 * ones(1500, 1); 3 * ones(1500, 1)];
Q = [randn(500, 4); randn(500, 4) + 3; randn(500, 4) - 3];
mdl = fitcknn(X, Y, 'NumNeighbors', 5, 'Standardize', true);
tic();
[label, score] = predict(mdl, Q);
t = toc();
asserts.isequal(size(label), [1500 1]);
asserts.isequal(size(score), [1500 3]);
asserts.istrue(t >= 0);
%=============================================================================
