%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--REUSE PROCESS-->
%=============================================================================
n = 1000000;
x = linspace(0.001, 40, n);
tic(); y = gampdf(x, 4, 2); t1 = toc();
tic(); p = gamcdf(x, 4, 2); t2 = toc();
tic(); q = gaminv(linspace(0.001, 0.999, 20000), 4, 2); t3 = toc();
tic(); ey = exppdf(x, 3); t4 = toc();
tic(); ep = expcdf(x, 3); t5 = toc();
tic(); eq = expinv(linspace(0.001, 0.999, 20000), 3); t6 = toc();
asserts.isequal(size(y), size(x));
asserts.isequal(size(p), size(x));
asserts.isequal(size(q), [1 20000]);
asserts.isequal(size(ey), size(x));
asserts.isequal(size(ep), size(x));
asserts.isequal(size(eq), [1 20000]);
disp(['gampdf observations: ', int2str(n), ', time: ', num2str(t1), ' s']);
disp(['gamcdf observations: ', int2str(n), ', time: ', num2str(t2), ' s']);
disp(['gaminv observations: 20000, time: ', num2str(t3), ' s']);
disp(['exppdf observations: ', int2str(n), ', time: ', num2str(t4), ' s']);
disp(['expcdf observations: ', int2str(n), ', time: ', num2str(t5), ' s']);
disp(['expinv observations: 20000, time: ', num2str(t6), ' s']);
%=============================================================================
