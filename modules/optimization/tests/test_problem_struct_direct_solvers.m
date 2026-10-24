%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
problem = struct();
problem.objective = @(x) sum((x(:) - [1; -2]) .^ 2);
problem.x0 = [0; 0];
problem.options = optimset('TolX', 1e-7, 'TolFun', 1e-7);
[x, fval, exitflag] = fminsearch(problem);
asserts.istrue(max(abs(x(:) - [1; -2])) < 1e-3);
asserts.istrue(abs(fval) < 1e-6);
asserts.istrue(exitflag > 0);
%=============================================================================
problem = struct();
problem.objective = @(x)(x - 1.5) .^ 2;
problem.x1 = -2;
problem.x2 = 4;
problem.options = optimset('TolX', 1e-8);
[x, fval, exitflag] = fminbnd(problem);
asserts.isapprox(x, 1.5, 1e-3);
asserts.istrue(abs(fval) < 1e-6);
asserts.istrue(exitflag > 0);
%=============================================================================
problem = struct();
problem.objective = @(x) x .^ 2 - 4;
problem.x0 = [0 5];
problem.options = optimset('TolX', 1e-8);
[x, fval, exitflag] = fzero(problem);
asserts.isapprox(x, 2, 1e-6);
asserts.istrue(abs(fval) < 1e-7);
asserts.istrue(exitflag > 0);
%=============================================================================
