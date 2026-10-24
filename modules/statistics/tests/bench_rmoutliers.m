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
X = randn(20000, 10);
X(50:2000:end, :) = 25;
tic();
[B, TFrm, TFoutlier] = rmoutliers(X, 'median');
t = toc();
asserts.isequal(size(B, 2), 10);
asserts.isequal(size(TFrm), [20000 1]);
asserts.isequal(size(TFoutlier), size(X));
asserts.istrue(t >= 0);
%=============================================================================
