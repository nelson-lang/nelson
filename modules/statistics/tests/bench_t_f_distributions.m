%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--REUSE PROCESS-->
%=============================================================================
n = 1000000;
x = linspace(-8, 8, n);
v = 12;
tic();
y = tpdf(x, v);
tPdf = toc();
tic();
p = tcdf(x, v);
tCdf = toc();
tic();
q = tinv(linspace(0.001, 0.999, 20000), v);
tInv = toc();
xf = linspace(0.001, 20, n);
tic();
fy = fpdf(xf, 5, 20);
fPdf = toc();
tic();
fp = fcdf(xf, 5, 20);
fCdf = toc();
tic();
fq = finv(linspace(0.001, 0.999, 20000), 5, 20);
fInv = toc();
asserts.isequal(size(y), size(x));
asserts.isequal(size(p), size(x));
asserts.isequal(size(q), [1 20000]);
asserts.isequal(size(fy), size(xf));
asserts.isequal(size(fp), size(xf));
asserts.isequal(size(fq), [1 20000]);
disp(['tpdf observations: ', int2str(n), ', time: ', num2str(tPdf), ' s']);
disp(['tcdf observations: ', int2str(n), ', time: ', num2str(tCdf), ' s']);
disp(['tinv observations: 20000, time: ', num2str(tInv), ' s']);
disp(['fpdf observations: ', int2str(n), ', time: ', num2str(fPdf), ' s']);
disp(['fcdf observations: ', int2str(n), ', time: ', num2str(fCdf), ' s']);
disp(['finv observations: 20000, time: ', num2str(fInv), ' s']);
%=============================================================================
