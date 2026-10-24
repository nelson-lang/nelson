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
n = 1000000;
x = linspace(0.001, 20, n);
v = 5;
tic();
y = chi2pdf(x, v);
tPdf = toc();
tic();
p = chi2cdf(x, v);
tCdf = toc();
tic();
q = chi2inv(linspace(0.001, 0.999, 20000), v);
tInv = toc();
asserts.isequal(size(y), size(x));
asserts.isequal(size(p), size(x));
asserts.isequal(size(q), [1 20000]);
disp(['chi2pdf observations: ', int2str(n), ', time: ', num2str(tPdf), ' s']);
disp(['chi2cdf observations: ', int2str(n), ', time: ', num2str(tCdf), ' s']);
disp(['chi2inv observations: 20000, time: ', num2str(tInv), ' s']);
%=============================================================================
