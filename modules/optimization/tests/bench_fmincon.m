%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fun = @(x) 100 * (x(2) - x(1) ^ 2) ^ 2 + (1 - x(1)) ^ 2;
opts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp', 'MaxIterations', 200);
[x, fval] = fmincon(fun, [0; 0], [], [], [], [], [], [], @optimTestFminconUnitDisk, opts);
asserts.istrue(max(abs(x(:) - [0.7864; 0.6177])) < 5e-3);
%=============================================================================
