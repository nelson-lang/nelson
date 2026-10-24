%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tic
fun = @(x) 3 * x(1) ^ 2 + 2 * x(1) * x(2) + x(2) ^ 2 - 4 * x(1) + 5 * x(2);
opts = optimoptions('fminunc', 'Display', 'off', 'MaxIterations', 100);
[x, fval] = fminunc(fun, [1, 1], opts);
toc
asserts.istrue(max(abs(x(:) - [2.25; -4.75])) < 1e-4);
asserts.istrue(abs(fval + 16.375) < 1e-6);
%=============================================================================
