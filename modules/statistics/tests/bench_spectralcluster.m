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
rng(9);
X = [randn(120, 2); randn(120, 2) + 4; randn(120, 2) + [-4 3]];
tic();
[idx, V, D] = spectralcluster(X, 3, 'NumNeighbors', 20, 'KernelScale', 'auto');
t = toc();
asserts.isequal(size(idx), [size(X, 1) 1]);
asserts.isequal(size(V), [size(X, 1) 3]);
asserts.isequal(size(D), [3 1]);
asserts.istrue(t >= 0);
%=============================================================================
