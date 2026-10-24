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
x = mod(0:n - 1, 120);
tic(); y = poisspdf(x, 35); t1 = toc();
tic(); p = poisscdf(x, 35); t2 = toc();
tic(); q = poissinv(linspace(0.001, 0.999, 20000), 35); t3 = toc();
asserts.isequal(size(y), size(x));
asserts.isequal(size(p), size(x));
asserts.isequal(size(q), [1 20000]);
disp(['poisspdf observations: ', int2str(n), ', time: ', num2str(t1), ' s']);
disp(['poisscdf observations: ', int2str(n), ', time: ', num2str(t2), ' s']);
disp(['poissinv observations: 20000, time: ', num2str(t3), ' s']);
%=============================================================================
