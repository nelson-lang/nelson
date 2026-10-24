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
x = randn(3000, 12);
tic();
[lambda, psi, T, stats, scores] = factoran(x, 3);
t = toc();
asserts.isequal(size(lambda), [12 3]);
asserts.isequal(size(psi), [12 1]);
asserts.isequal(size(T), [3 3]);
asserts.isequal(size(scores), [3000 3]);
asserts.istrue(isfield(stats, 'residual'));
asserts.istrue(t < 10);
%=============================================================================
