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
x = linspace(0.001, 0.999, n);
tic(); y = betapdf(x, 2, 5); t1 = toc();
tic(); p = betacdf(x, 2, 5); t2 = toc();
tic(); q = betainv(linspace(0.001, 0.999, 20000), 2, 5); t3 = toc();
asserts.isequal(size(y), size(x));
asserts.isequal(size(p), size(x));
asserts.isequal(size(q), [1 20000]);
disp(['betapdf observations: ', int2str(n), ', time: ', num2str(t1), ' s']);
disp(['betacdf observations: ', int2str(n), ', time: ', num2str(t2), ' s']);
disp(['betainv observations: 20000, time: ', num2str(t3), ' s']);
%=============================================================================
