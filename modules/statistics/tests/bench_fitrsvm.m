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
X = randn(300, 4);
y = 2 * X(:, 1) - X(:, 2) + 0.5 * X(:, 3) .^ 2;
Q = randn(120, 4);
tic();
mdl = fitrsvm(X, y, 'KernelFunction', 'gaussian', 'KernelScale', 2, ...
  'BoxConstraint', 5, 'Standardize', true);
yfit = predict(mdl, Q);
t = toc();
asserts.isequal(size(yfit), [120 1]);
asserts.istrue(t >= 0);
%=============================================================================
