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
n = 1000000;
x = linspace(-2, 4, n);
tic(); y = unifpdf(x, -1, 3); t1 = toc();
tic(); p = unifcdf(x, -1, 3); t2 = toc();
tic(); q = unifinv(linspace(0.001, 0.999, n), -1, 3); t3 = toc();
asserts.isequal(size(y), size(x));
asserts.isequal(size(p), size(x));
asserts.isequal(size(q), size(x));
disp(['unifpdf observations: ', int2str(n), ', time: ', num2str(t1), ' s']);
disp(['unifcdf observations: ', int2str(n), ', time: ', num2str(t2), ' s']);
disp(['unifinv observations: ', int2str(n), ', time: ', num2str(t3), ' s']);
%=============================================================================
