%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = optimvar('x');
prob = optimproblem('Objective', (x - 2) .^ 2);
[sol, fval, exitflag, output] = solve(prob, struct('x', 0), optimset('TolX', 1e-7, 'TolFun', 1e-7));
asserts.isapprox(sol.x, 2, 1e-3);
asserts.isapprox(fval, 0, 1e-6);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
%=============================================================================
