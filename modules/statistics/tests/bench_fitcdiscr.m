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
X = [randn(2500, 5); randn(2500, 5) + 3; randn(2500, 5) - 3];
Y = [ones(2500, 1); 2 * ones(2500, 1); 3 * ones(2500, 1)];
Q = [randn(1000, 5); randn(1000, 5) + 3; randn(1000, 5) - 3];
mdl = fitcdiscr(X, Y, 'DiscrimType', 'linear');
tic();
[label, score] = predict(mdl, Q);
t = toc();
asserts.isequal(size(label), [3000 1]);
asserts.isequal(size(score), [3000 3]);
asserts.istrue(t >= 0);
%=============================================================================
